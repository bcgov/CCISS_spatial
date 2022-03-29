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
    mapInputs$gcm <- input$col1_gcm
    mapInputs$scn <- input$col1_scn
    mapInputs$time <- input$time
    
    
    if(input$type == 1){
      
      #translate futureperiod 
      mapInputs$bgctime <- switch(input$time, "2001-2020" = "2001",
                                              "2021-2040" = "2021",
                                              "2041-2060" = "2041",
                                              "2061-2080" = "2061",
                                              "2081-2100" = "2081")
      
    }
    
    
    if(input$type == 2){
      mapInputs$feastime <- switch(input$time, "2001-2020" = 1,
                                              "2021-2040" = 2,
                                              "2041-2060" = 3,
                                              "2061-2080" = 4,
                                              "2081-2100" = 5)
      mapInputs$sppPick <- spp$species_id[spp$species  == input$sppPick]
      mapInputs$edaPick <- switch(input$edaPick,"B2"=11,"C4"=21,"E6"=39)
      mapInputs$feasType <- input$feasType
    }
    
     mapInputs$type <- input$type
    
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
    
    #reset raster values
    bc_raster <- raster::setValues(bc_raster,NA)
    
    if(nrow(dat)>0) {
      
    if(mapInputs$type == 1){
    
    incProgress(0.6)
      
      bgcs <- unique(dat$bgc_pred)
      bgcID <- data.table(bgc = bgcs, id = 1:length(bgcs))
      
      #dat[bgc_colors,Col := i.Col, on = c(bgc_pred = "BGC")]
      dat[bgcID,bgcID := i.id, on = c(bgc_pred = "bgc")]

      bc_raster[dat$rast_id] <- dat$bgcID
      bc_raster <- ratify(bc_raster)
      
      bgcID[bgc_colors,Col := i.Col, on = c(bgc = "BGC")]
      pal <- colorFactor(palette = bgcID$Col,bgcID$id, na.color = "transparent" )
    
    }
      
    if(mapInputs$type == 2){
      
      #retrieve species feasibility
      sppFeas <- dbGetQuery(pool_dev, paste0("select siteno, curr, newsuit from pts2km_feas
                                              where futureperiod_id = ", mapInputs$feastime,
                                              "and edatope_id = ", mapInputs$edaPick,
                                              "and species_id = ", mapInputs$sppPick))%>%
                 setDT()
      
      incProgress(0.6, detail = "Retriving species feasibility results")
      
      sppFeas <- dat[sppFeas, on = .(rast_id = siteno)]
      
      
      feas_cols <- data.table(Suit = c(1,2,3),Col = c("#0c8a32","#43a7e0","#db3700"))
      change_cols <- data.table(Diff = c(-3,-2,-1,0,1,2,3),
                                Col = c("#ff1900","#ff6633","#f78952","#ffffff","#69cfff","#3b9dff","#002aff"))
      
      if(mapInputs$feasType == "RawVotes"){
        sppFeas[,Col := colour_values(newsuit, palette = "viridis")]
        
        bc_raster[sppFeas$rast_id] <- sppFeas$newsuit
        bc_raster <- ratify(bc_raster)
        pal <- colorNumeric("viridis",values(bc_raster), na.color = "transparent" )
        
      }else if(mapInputs$feasType == "Feasibility"){
        sppFeas[,newsuit := round(newsuit)]
        sppFeas <- sppFeas[newsuit < 3.5,]
        #sppFeas[feas_cols,Col := i.Col, on = c(newsuit = "Suit")]
        bc_raster[sppFeas$rast_id] <- sppFeas$newsuit
        bc_raster <- ratify(bc_raster)
        pal <- colorFactor(c("#0c8a32","#43a7e0","#e8e531"),c(1,2,3), na.color = "transparent" )
        
      }else if(mapInputs$feasType == "Change"){
        sppFeas[,newsuit := round(newsuit)]
        sppFeas <- sppFeas[newsuit < 4.5,]
        sppFeas[,Diff := curr - newsuit]
        #sppFeas[change_cols, Col := i.Col, on = "Diff"]
        
        bc_raster[sppFeas$rast_id] <- sppFeas$Diff
        bc_raster <- ratify(bc_raster)
        pal <- colorFactor(c("#ff1900","#ff6633","#f78952","#ffffff","#69cfff","#3b9dff","#002aff"),
                           c(-3,-2,-1,0,1,2,3), 
                           na.color = "transparent" )
      }
      
    }
      
      
    }else{
      
      bc_raster <- bc_raster
      bc_raster_cols <- pal
    }
      
    
      
    })
    
    return(list(bc_raster = bc_raster,
                bc_raster_cols = pal))
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
    
      
    if(mapInputs$type == 1) {
      
      incProgress(0.6, detail = "Retrieving BGC projection")
      
      #load BGC projections
      dat <- dbGetCCISSRaw2(pool,poly ,mapInputs$gcm,mapInputs$scn,mapInputs$bgctime)
      
      
      if(nrow(dat)>0){
        
        incProgress(0.8)
        
        dat[bgc_colors,Col := i.Col, on = c(bgc_pred = "BGC")]
        dat <- st_as_sf(dat)
        dat <- st_transform(dat, 4326)
      
      }else{
      
      dat <- NULL
      }
      
    }
      
    if(mapInputs$type ==2){
        
    }

      
   
    return(dat)
    })
    
  })
  
  
observe({
    
   req(mapInputs$zoomlevel)
    
    #retrieve data based on zoom level, 2Km grid if zoom level below 12, 400m grid if zoom level above 12
    if(mapInputs$zoomlevel == 1){
      
      mapData$mapData <- rasterlayer()$bc_raster
      mapData$mapCol <- rasterlayer()$bc_raster_cols
     
    }
    
    
    if(mapInputs$zoomlevel == 2){
    
      mapData$mapData <- vectorlayer()
      
    }
    
  })
  
  output$zoomlevel_display <- renderUI({
    
    HTML(paste0("<b>Zoom level: ",mapInputs$map_zoom_init,"</b>",
         "<p>Data change to 400m grid after zoom level 10</p>"))
    
    })
  
  
  output$map <- renderLeaflet({
    
    leaflet(options = leafletOptions(minZoom = 5, maxZoom = 12)) %>%
      #addTiles()%>%
      addProviderTiles("Esri.WorldStreetMap", group= "Esri")%>%
      addScaleBar(position = "bottomleft") %>%
      setView(lng = -126.5, lat = 54.5, zoom = 5)%>%
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
      clearControls() %>%
      #fitBounds(lng1 = dist_boundary()$lng1, lat1 = dist_boundary()$lat1,lng2 = dist_boundary()$lng2, lat2 = dist_boundary()$lat2) %>%
      addRasterImage(mapData$mapData, 
                     colors = mapData$mapCol, 
                     opacity = input$opacity,
                     layerId = "raster")%>%
      stopSpinner()
    
    if(mapInputs$type == 2){
      
      if(mapInputs$feasType == "RawVotes" ) {
      leafletProxy("map") %>%
        addLegend(pal = mapData$mapCol, values = c(1:5),
                  title = "Raw Votes")
        
      }else if(mapInputs$feasType == "Feasibility"){
        leafletProxy("map") %>%
        addLegend(pal = mapData$mapCol, values = c(1,2,3),
                  title = "Climatic feasibility")
      }else {
        leafletProxy("map") %>%
        addLegend(pal = mapData$mapCol, values = c(-3,-2,-1,0,1,2,3),
                  title = "Mean change")
      }
    }

  }

})


observeEvent(input$subarea, {

if(input$subarea != "None") {
  
  if(input$subarea == "User upload"){
    poly_diff <- user_upload$outputs$poly_diff
  }
  
  if(input$subarea %in% districts){
    
    poly_diff <-bc_districts%>%
      dplyr::filter(ORG_UNIT == input$subarea)%>%
      st_transform(4326)
    
    bounding_box <- st_bbox(c(xmin = -160, xmax = -90, ymax = 70, ymin = 40), crs = st_crs(4326))%>%
      st_as_sfc()%>%
      st_as_sf()
    
    st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
    #inverted polygon
    poly_diff <- st_erase(bounding_box,poly_diff)
    
  }
  
  leafletProxy("map") %>%
    removeShape(layerId = "masking") %>%
    addPolygons(data = poly_diff,color = "transparent", fillColor = "#D3D3D3" , fillOpacity = 0.8, layerId = "masking")
  
}else{
  
  leafletProxy("map") %>%
    removeShape(layerId = "masking")
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
    
    futureperiod <- mapInputs$time
    scn <- mapInputs$scn
    
    withProgress(message = "Retrieving climate data from database", value = 0, {
    
    query <- paste0("select a.scenario, a.period, a.bgc, a.climvar, max(a.value) as value, b.value as reference
                 from szsum_fut a
                 join (select bgc,climvar, value from szsum_curr 
                       where period = '1961 - 1990'
                       and stat = 'mean') as b
                 on a.bgc = b.bgc
                 and a.climvar = b.climvar
                 where a.period = '",futureperiod,"' 
                 and a.scenario = '",scn,"' 
                 and a.climvar IN ('MAT','Tave','MCMT','TD','EMT')
                 and a.stat = 'mean'
                 group by a.scenario, a.period, a.bgc, a.climvar,b.value")
    
    climdata <-dbGetQuery(pool2, query)
    
    incProgress(0.8)
    
    climdata <- setDT(climdata)
    climdata[, change := value - reference]
      
    })
    
    return(climdata)
  })
  
  
  plotData <- reactive({

  if(input$subarea == "None"){
      
  dat <- NULL
    
  }else {
      
  if(mapInputs$type == 1){
     
     if (input$subarea == "User upload"){
       dat <- user_upload$outputs$bgc
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
    
  
   if(mapInputs$type == 2){
     dat <- NULL
   }
}
    
   return(dat)
    
  })
  
  observeEvent(plotData(),{
    
    subzones <- unique(plotData()$bgc)
    updateSelectizeInput(session, "subzone", choices = subzones, selected = subzones[1])
  })
  
  output$scatterplot<- renderPlotly({
    
    dat <- plotData()
    
    validate(
      need(nrow(dat)>0, "Please select a region")
    )
    
    #filter data based on x axis selection
    dat <- dat[climvar == input$var1 & bgc %in% input$subzone]
    
    plot_ly(dat, x = ~ change, y = ~ bgc_area, color = ~ gcm,colors = colorRampPalette(brewer.pal(8, "Spectral"))(13),
            type = "scatter", mode = "markers")%>%
      layout(xaxis = list(title = paste0("Change in ", input$var1)),
             yaxis = list(title = "Area of biogeoclimatic units (sq km)"),
             legend = list(title=list(text='<b> GCM </b>')))
    
  })
  
  
  output$climatevarplot <- renderPlotly({
    
    climdata <- climateData()
    
    xvar <- climdata[climvar == input$var1]
    yvar <- climdata[climvar == input$var2]
    
    dat <- xvar[yvar, on = 'bgc']
    
    plot_ly(dat, x = ~ change, y = ~ i.change, type = "scatter", mode = "markers")%>%
      layout(xaxis = list(title = paste0("Change in ",input$var1,"\n", climvars_label$Variable[climvars_label$Code == input$var1])),
             yaxis = list(title = paste0("Change in ",input$var2,"\n", climvars_label$Variable[climvars_label$Code == input$var2])))
  })
  
  

  user_upload <- callModule(uploadFileServer,"uploadfile")
  
  observeEvent(user_upload,{
    
    add_choice <- user_upload$filename()
    updateSelectInput(session, "subarea", choices = c("None",add_choice, districts))
  })
  
  # output$test_tb <- DT::renderDT({
  #   req(user_upload)
  #   
  #   df <- user_upload$outputs$bgc
  #   datatable(
  #   df%>%as.data.frame()
  #   )
  # })
  

}