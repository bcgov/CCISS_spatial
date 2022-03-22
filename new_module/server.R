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
    
    
    if(input$dist == "All BC"){
      mapInputs$dist <- districts
    }else{
      mapInputs$dist <- input$dist
    }
    
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
    
    #look up hex_grid table from boundbox:
    dat <- st_read(pool,query = paste0("select siteno,geom from hex_points where st_intersects(geom, 'SRID=3005;",poly,"');"))
    
    
    
    if(nrow(dat) > 0){
      
      incProgress(0.6, detail = "Retrieving BGC projection")
      
      #load BGC projections
      bgc_predictions <- dbGetCCISSRaw(pool,dat$siteno,mapInputs$gcm,mapInputs$scn,mapInputs$bgctime)
      
      incProgress(0.8)
      # dat <- dat %>%
      #        left_join(bcg_predictions[, c('siteno', 'bgc_pred')])%>%
      #        left_join(bgc_colors, by = c('bgc_pred' = 'BGC'))%>%
      #        st_transform(crs = 4326)
      
      #use data table for merge to speed up
      dat <- setDT(dat)
      dat <- dat[bgc_predictions, on = "siteno"]
      dat[bgc_colors,Col := i.Col, on = c(bgc_pred = "BGC")]
      dat <- st_as_sf(dat)
      dat <-st_transform(dat, 4326)

      
    }else{
      
      dat <- NULL
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
  
  dist_center <- reactive({
    
    if(input$dist == "All BC"){
      
      lng1 <- -140
      lat1 <- 60
      lng2 <- -118
      lat2 <- 47
      
    }else{
      
      boundary <- unname(dist_bbox$bb[dist_bbox$ORG_UNIT == input$dist][[1]])
      lng1 <- boundary[1]
      lat1 <- boundary[2]
      lng2 <- boundary[3]
      lat2 <- boundary[4]
      
    }
    
    list(lng1 = lng1, lat1=lat1, lng2 = lng2, lat2= lat2)
  })
  
  
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
      addCircleMarkers(
        fillOpacity = 0.7,
        color = ~ Col,
        radius = 5
      )%>%
      stopSpinner()

  }else{

    leafletProxy("map") %>%
      startSpinner(list("lines" = 7, "length" = 40, "width" = 20, "radius" = 5)) %>%
      clearMarkers() %>%
      #fitBounds(lng1 = dist_boundary()$lng1, lat1 = dist_boundary()$lat1,lng2 = dist_boundary()$lng2, lat2 = dist_boundary()$lat2) %>%
      addRasterImage(mapData$mapData, colors = mapData$mapCol$Col, opacity = 0.8, layerId = "raster")%>%
      stopSpinner()

  }

  
    
    

  })
  
  
  plotData <- reactive({
    
    #retrieve climate variables
    mtcars
  })
  
  output$scatterplot<- renderPlotly({
    
    p <- ggplot(plotData(), aes(wt, mpg))
    p <-  p + geom_point()
    
    ggplotly(p)
    
  })
  
  
  output$climatevarplot <- renderPlotly({
    
    p <- ggplot(mtcars, aes(wt, mpg))
    p <-  p + geom_point()
    
    ggplotly(p)
  })


   
}