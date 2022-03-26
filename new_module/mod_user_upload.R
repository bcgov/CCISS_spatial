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
        shpFile <- st_read(shpPath)
        
        return(shpFile)
        })
      
      
  outputs <- reactiveValues(bgc = NULL)
      
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
        title = "Calculate species feasibility area... This step might take several minutes",
        value = 40
      )
      
      #calculate species feasibility area
      
      
      
      
      
      
      
      
      closeSweetAlert(session = session)
      sendSweetAlert(
        session = session,
        title =" Upload complete !",
        type = "success"
      )
      
      
      
      })
      
      return(list(
                  filename = reactive("User upload"),
                  bgc_area = outputs))
      
}
