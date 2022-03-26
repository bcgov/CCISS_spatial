server <- function(input, output, session) {
  
  #set initial value
  mapInputs <- reactiveValues(map_zoom_init = 5)
  
  
  observe({
    req(mapInputs$map_zoom_init)
    
    if(mapInputs$map_zoom_init < 11){
      
      level <- 1
    }else{
      
      level <- 2
    }
    
    mapInputs$zoomlevel <- level
    
    
    # if(input$dist == "All BC"){
    #   mapInputs$dist <- districts
    # }else{
    #   mapInputs$dist <- input$dist
    # }
    
    if(input$type == 1){
      
      mapInputs$gcm <- input$col1_gcm
      mapInputs$scn <- input$col1_scn
      mapInputs$time <- input$time
      
      #translate futureperiod 
      mapInputs$bgctime <- switch(input$time, "2001-2020" = "2001",
                                              "2021-2040" = "2021",
                                              "2041-2060" = "2041",
                                              "2061-2080" = "2061",
                                              "2081-2100" = "2081")
      
    }
    
    
    if(input$type ==2){
      
    }
    
  })
  
  observe({
    
    mapInputs$map_zoom_init <- input$map_zoom
    mapInputs$map_bounds <- input$map_bounds
    
  })
  
  
  #only query climate y axis data when switch is on
  observeEvent(input$showclimate,{
    
    shinyjs::toggle("var2")
    shinyjs::toggle("climatevarplot")
    
  })
  
  mapData <- reactiveValues(mapData = NULL,
                            mapCol = NULL)
  
  
  
  
  #cache results for raster layer in reactive
  rasterlayer <- reactive({

    withProgress(message = "Retrieving 2Km grid data from database", value = 0, {
      
    if(input$type == 1){
    
    dat <- dbGetbgc_raster(pool, mapInputs$time, mapInputs$scn, mapInputs$gcm)
    
    incProgress(0.6)
    
    if(nrow(dat)>0) {
      
      bgcs <- unique(dat$bgc_pred)
      bgcID <- data.table(bgc = bgcs, id = 1:length(bgcs))
      
      dat[bgc_colors,Col := i.Col, on = c(bgc_pred = "BGC")]
      dat[bgcID,bgcID := i.id, on = c(bgc_pred = "bgc")]
      bc_raster <- raster::setValues(bc_raster,NA)
      bc_raster[dat$rast_id] <- dat$bgcID
      bc_raster <- ratify(bc_raster)
      
      bgcID[bgc_colors,Col := i.Col, on = c(bgc = "BGC")]
      
    }else{
      
      bc_raster <- bc_raster
      bgcID <- NULL
    }
    
    }
      
    if(input$type ==2){
      
    }
      
    })
    
    return(list(bc_raster = bc_raster,
                bgcID = bgcID))
  })
  
  
  vectorlayer <- reactive({

    
    withProgress(message = "Retrieving 400m grid data from database", value = 0, {
      
    
    bounds <- mapInputs$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    poly_df <- data.frame(lon = lngRng, lat = latRng)
    
    poly <- st_as_sf(poly_df, coords = c("lon", "lat"), crs = 4326)%>%
      st_transform(3005)%>%
      st_bbox() %>% 
      st_as_sfc()%>%
      st_as_text()
    
    incProgress(0.6, detail= "Retrieving BGC projection")
    
      
    if(input$type == 1) {
      
      incProgress(0.6, detail = "Retrieving BGC projection")
      
      #load BGC projections
      dat <- dbGetCCISSRaw2(pool,poly ,mapInputs$gcm,mapInputs$scn,mapInputs$bgctime)
      
      
      if(nrow(dat)>0){
        
        incProgress(0.8)
        
        dat[bgc_colors,Col := i.Col, on = c(bgc_pred = "BGC")]
        dat <- st_as_sf(dat)
        dat <-st_transform(dat, 4326)
      
      }else{
      
      dat <- NULL
      }
      
    }
      
    if(input$type ==2){
        
    }

      
   
    return(dat)
    })
    
  })
  
  
observe({
    
   req(mapInputs$zoomlevel)
    
    #retrieve data based on zoom level, 2Km grid if zoom level below 12, 400m grid if zoom level above 12
    if(mapInputs$zoomlevel == 1){
      
      mapData$mapData <- rasterlayer()$bc_raster
      mapData$mapCol <- rasterlayer()$bgcID
     
    }
    
    
    if(mapInputs$zoomlevel == 2){
    
      mapData$mapData <- vectorlayer()
      
    }
    
  })
  
  output$zoomlevel_display <- renderText(mapInputs$map_zoom_init)
  
  # dist_center <- reactive({
  #   
  #   if(input$dist == "All BC"){
  #     
  #     lng1 <- -140
  #     lat1 <- 60
  #     lng2 <- -118
  #     lat2 <- 47
  #     
  #   }else{
  #     
  #     boundary <- unname(dist_bbox$bb[dist_bbox$ORG_UNIT == input$dist][[1]])
  #     lng1 <- boundary[1]
  #     lat1 <- boundary[2]
  #     lng2 <- boundary[3]
  #     lat2 <- boundary[4]
  #     
  #   }
  #   
  #   list(lng1 = lng1, lat1=lat1, lng2 = lng2, lat2= lat2)
  # })
  
  
  output$map <- renderLeaflet({
    
    leaflet(options = leafletOptions(minZoom = 5, maxZoom = 12)) %>%
      #addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron")%>%
      #addProviderTiles("OpenStreetMap.Mapnik", group= "OpenStreetMap")%>%
      addProviderTiles("Esri.WorldStreetMap", group= "Esri")%>%
      addScaleBar(position = "bottomleft") %>%
      setView(lng = -126.5, lat = 54.5, zoom = 5)%>%
      # addLayersControl(
      # baseGroups = c("CartoDB.Positron", "OpenStreetMap", "Esri"),
      # options = layersControlOptions(collapsed = TRUE)
      # )%>%
      addSpinner()
    
  })
  
  
observe({
    req(mapData$mapData)
    #req(mapInputs$map_zoom_init)
  

  if(mapInputs$zoomlevel == 2 ){

    leafletProxy("map", data = mapData$mapData) %>%
      startSpinner(list("lines" = 7, "length" = 40, "width" = 20, "radius" = 5)) %>%
      clearImages() %>%
      clearMarkers() %>%
      addCircleMarkers(
        fillOpacity = input$opacity,
        color = ~ Col,
        radius = 5,
        label = ~ bgc_pred
      )%>%
      stopSpinner()

  }else{

    leafletProxy("map") %>%
      startSpinner(list("lines" = 7, "length" = 40, "width" = 20, "radius" = 5)) %>%
      clearMarkers() %>%
      #fitBounds(lng1 = dist_boundary()$lng1, lat1 = dist_boundary()$lat1,lng2 = dist_boundary()$lng2, lat2 = dist_boundary()$lat2) %>%
      addRasterImage(mapData$mapData, 
                     colors = mapData$mapCol$Col, 
                     opacity = input$opacity,
                     layerId = "raster")%>%
      stopSpinner()

  }

})


#Show popup on click
# observeEvent(input$map_click, {
# 
#   if(mapInputs$zoomlevel == 1 ){
# 
# 
#   click <- input$map_click
#   xy <- SpatialPoints(data.frame(click$lng, click$lat))
#   proj4string(xy) <- ""
#   leafletProj <- "+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"
#   xy <- as.data.frame(spTransform(xy, leafletProj))
#   #Get the cell number from the newly transformed metric X and Y.
#   cell <- cellFromXY(depth, c(xy$x, xy$y))
#   
#   dat <- mapData$mapData
#   bgcID <- mapData$mapCol
#   cellnum <- dat[cellFromXY(dat, matrix(c(click$lng, click$lat), 1))]
#   bgc.popup <- bgcID$bgc[bgcID$id == values(dat)[cellnum]]
#   text<-paste0("<strong>", bgc.popup, "</strong>")
# 
#   leafletProxy("map") %>%
#     clearPopups() %>%
#     addPopups(click$lng, click$lat, text, options = popupOptions(closeButton = FALSE, closeOnClick = TRUE))
# 
#   }
# 
# })

  climateData <- reactive({
    
    withProgress(message = "Retrieving climate data from database", value = 0, {
    
    if(input$subarea == "None"){
     climdata <- NULL
     
    }else{
    
    query <- paste0("
                    select bgc, value, climvar
                    from szsum_fut
                    where period = '",mapInputs$time,
                    "' and scenario = '",mapInputs$scn,
                    "' and climvar IN ('MAT','MAR','MSP','CMD','SHM')
                       and stat = 'mean'")
    
    climdata <-dbGetQuery(pool2, query)
    
    incProgress(0.8)
    
    climdata <- setDT(climdata)
    }
      
    })
    
    return(climdata)
  })
  
  
  plotData <- reactive({
    
   if(input$type == 1){
     
     if(input$subarea == "None"){
       dat <- NULL
       
     }else if(input$subarea == "User upload"){
       dat <- user_upload$bgc_area$bgc
       climdata <- climateData()
       
       #filter bgc count by time period and scenario 
       dat <- dat[futureperiod == mapInputs$bgctime & scenario == mapInputs$scn]
       
       #join climvar to bgc count
       dat <- climdata[dat, on = .(bgc == bgc_pred)]
       
     }else{
       #TODO future implementation of regions and districts
       dat <- NULL
     }
     
   }
    
    
    
   return(dat)
    
  })
  
  observeEvent(plotData(),{
    
    subzones <- unique(plotData()$bgc)
    updateSelectizeInput(session, "subzone", choices = subzones)
  })
  
  output$scatterplot<- renderPlotly({
    
    dat <- plotData()
    
    validate(
      need(nrow(dat)>0, "Please select a region")
    )
    
    #filter data based on x axis selection
    dat <- dat[climvar == input$var1 & bgc %in% input$subzone]
    
    plot_ly(dat, x = ~ value, y = ~ bgc_area, color = ~ gcm,colors = colorRampPalette(brewer.pal(8, "Spectral"))(13),
            type = "scatter", mode = "markers")%>%
      layout(xaxis = list(title = input$var1),
             yaxis = list(title = "Area of biogeoclimatic units (sq km)"),
             legend = list(title=list(text='<b> GCM </b>')))
    
  })
  
  
  output$climatevarplot <- renderPlotly({
    
    climdata <- climateData()
    
    p <- ggplot(mtcars, aes(wt, mpg))
    p <-  p + geom_point()
    
    ggplotly(p)
  })

  user_upload <- callModule(uploadFileServer,"uploadfile")
  
  observe({
    
    add_choice <- user_upload$filename()
    updateSelectInput(session, "subarea", choices = c("None",add_choice, districts))
  })
  
  output$test_tb <- DT::renderDT({
    req(user_upload)
    
    df <- user_upload$bgc_area$bgc
    datatable(
    df%>%as.data.frame()
    )
  })

}