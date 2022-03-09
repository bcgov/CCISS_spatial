server <- function(input, output, session) {
  
  mapInputs <- reactiveValues()
  
  observe({
    
    if(input$dist == "All BC"){
      mapInputs$dist <- districts
    }else{
      mapInputs$dist <- input$dist
    }
    
    if(input$type == 1){
      
      mapInputs$gcm <- input$col1_gcm
      mapInputs$scn <- input$col1_scn
      mapInputs$time <- input$time
      
    }
    
  })
  
  mapData <- reactive({
    
    withProgress(message = "Retrieving data from database", value = 0, {
    
    dat <- dbGetbgc(pool, mapInputs$time, mapInputs$scn, mapInputs$gcm, mapInputs$dist)
    
    if(nrow(dat)>0) {
    
    dat <- dat %>%
      left_join(bgc_colors, by = c('bgc_pred' = 'BGC'))
    
    incProgress(0.6, detail = "Convert to projection 4326")
    
    #apply correct projection
    dat <- st_transform(dat, crs = 4326)
    
    }else{
      
    dat <- data.frame(dist_code = character())
    }
    
    dat
    
    })
    
  })
  
  dist_boundary <- reactive({
    
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
    leaflet() %>%
      fitBounds(lng1 = dist_boundary()$lng1, lat1 = dist_boundary()$lat1,lng2 = dist_boundary()$lng2, lat2 = dist_boundary()$lat2) %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron")%>%
      addProviderTiles("OpenStreetMap.Mapnik", group= "OpenStreetMap")%>%
      addProviderTiles("Esri.WorldStreetMap", group= "Esri")%>%
      addScaleBar(position = "bottomleft") %>%
      addLayersControl(
      baseGroups = c("CartoDB.Positron", "OpenStreetMap", "Esri"),
      options = layersControlOptions(collapsed = TRUE)
      )%>%
      addSpinner()
    
  })
  
  observe({
    
    validate(
      need(nrow(mapData())>0 , "Please select another data set")
    )


    leafletProxy("map", data = mapData()) %>%
      startSpinner(list("lines" = 7, "length" = 40, "width" = 20, "radius" = 10)) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~ Col,
        color = ~ Col,
        label = ~ bgc_pred
      )%>%
      stopSpinner()
    

  })


   
}