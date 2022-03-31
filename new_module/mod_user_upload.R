#module for user uploaded area of interest


# Module UI function
uploadFileUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    useSweetAlert(),
    tags$div(tags$h5("Upload shape file", style = "font-weight: bold;"),
             tags$b("Please upload .dbf, .shx, .prj files along with .shp file"),
             tags$p("The upload step involves heavy calculation, it might take minutes to complete")
    ),
    fileInput(ns("shpfile"),label = "", accept = c('.shp', '.dbf','.sbn', '.sbx', '.shx', '.prj'), multiple = TRUE)
  )
}


# Module server function
uploadFileServer <- function(input, output, session) {
  
    ns <- session$ns
      #upload shape file along with prj, shx, dbf 
      
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$shpfile, message = FALSE))
        
        shpDF <- input$shpfile
        prevWD <- getwd()
        uploadDirectory <- dirname(shpDF$datapath[1])
        setwd(uploadDirectory)
        for (i in 1:nrow(shpDF)){
          file.rename(shpDF$datapath[i], shpDF$name[i])
        }
        shpName <- shpDF$name[grep(x=shpDF$name, pattern="*.shp")]
        shpPath <- paste(uploadDirectory, shpName, sep="/")
        setwd(prevWD)
        
        
        #add tryCatch error handling
        upload_shape_file <- function(datapath){
          
          events <- function(e) {
            showModal(
              modalDialog(
                title = "Invalid file",
                paste("Your shape file could not be read, please make sure to upload .shx, .proj, .dbf files together with .shp file. 
                      Zipped files are not accepted."),
                easyClose = TRUE
              )
            )
            return(NULL)
          }
          
          shpFile <- tryCatch({ st_read(datapath) }, error = events, warning = events)
  
          
          return(shpFile)
        }
        
        shpFile <- upload_shape_file(shpPath)
        
        
        #TODO: control area size of user upload
        #area_size <- as.numeric(st_area(shpFile))/1000000
        
        # if(area_size > 45000){
        #   
        #   showModal(
        #     modalDialog(
        #       title = "File too large to process",
        #       paste("The polygon from your shape file covers an area larger than 45000 sq km. It is too large to process your data at this moment"),
        #       easyClose = TRUE
        #     )
        #   )
        #   return(NULL)
        #   
        # }
        # 
        
        
        return(shpFile)
        })
      
      
  outputs <- reactiveValues(bgc = NULL,
                            poly_diff = NULL)
      
  observeEvent(userFile(),{
    
      #add progress bar alert
      progressSweetAlert(
        session = session, id = "myprogress",
        title = "Read shape file...",
        display_pct = TRUE, value = 0
      )
    
     
      # read shape file
      #st_join to subset BGC projection 400m grid results
      poly <- userFile()%>%
              st_transform(3005)%>%
              st_as_sfc() %>% 
              st_as_text()
      
      updateProgressBar(
        session = session,
        id = "myprogress",
        title = "Calculate BGC projection area... This step might take several minutes",
        value = 10
      )
      
      #calculate BGC area
      #count # grid by BGC projection
      
      bgc <- dbGetbgc_count(pool, poly)
      bgc$bgc_area <- bgc$n * 0.16
      
      outputs$bgc <- bgc
      
      updateProgressBar(
        session = session,
        id = "myprogress",
        title = "Calculate species feasibility area... This step is computatationally heavy and can take a while",
        value = 40
      )
      
      #calculate species feasibility area
      
      #retrieve raw BGC projections
      #bgc_feas <- dbGetCCISSRaw3(pool_dev, poly)
      
      updateProgressBar(
        session = session,
        id = "myprogress",
        title = "Calculate species feasibility area... This step is computatationally heavy and can take a while",
        value = 70
      )
      
      #retrieve feasibility score from user upload for all time period(2001=1, 2021=2, 2041=3, 2061=4, 2081=5), 
      #species (Bl = 10, Fd =18, Lw = 30, Pl = 50,Py = 54, Sx = 69)
      #edatopes(B2 =11, C4 =21, E6 =39) and 
      
      
      cciss_sql <- paste0("
    SELECT a.siteno,
           a.curr,
           a.newsuit,
           a.futureperiod_id,
           a.species_id,
           a.edatope_id
    FROM pts400m_feas a
    JOIN hex_points b
    ON a.siteno = b.siteno
    WHERE st_intersects(b.geom, 'SRID=3005;",poly,"')
    ")
      
      RPostgres::dbExecute(pool_dev,  "set enable_seqscan = off")
      sspFeas <- setDT(dbGetQuery(pool_dev,cciss_sql))
      
      RPostgres::dbExecute(pool_dev,  "set enable_seqscan = on")
    
      
      #filter out newsuit > 3
      sspFeas <- sspFeas[newsuit <= 3]
      
      
      #count number of grid by futureperiod, species_id, edatope_id
      sspFeas <- sspFeas[, keyby = .(futureperiod_id, species_id, edatope_id), .N]
      sspFeas$area <- sspFeas$N * 0.16
      
      outputs$sspFeas <- sspFeas
      
      #calculate invert polygon to gray out
      poly_diff <-userFile()%>%
                  st_transform(4326)
      
      bounding_box <- st_bbox(c(xmin = -160, xmax = -90, ymax = 70, ymin = 40), crs = st_crs(4326))%>%
        st_as_sfc()%>%
        st_as_sf()
      
      st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
      #inverted polygon
      poly_diff <- st_erase(bounding_box,poly_diff)
      
      outputs$poly_diff <- poly_diff
      
      
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title =" Upload complete !",
        type = "success"
      )
      
      
      
      }, ignoreNULL = TRUE)
      
      return(list(
                  filename = reactive("User upload"),
                  outputs = outputs))
      
}
