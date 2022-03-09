ui <- tagList(
  
  
  shinyjs::useShinyjs(),
  includeCSS("www/style.css"),
  
  navbarPage(
           
           title = div(img(src="logo.svg"), "The CCISS Tool"),
           theme = shinytheme("united"),
          
           tabPanel("App",
                    
                  #div(id = 'Sidebar',
                  #sidebarPanel(
                  column(8,
                  absolutePanel(top = 60, left = 5, right = "auto", bottom = "auto",
                                width = 300, height = "auto",draggable = TRUE,
                    
                    wellPanel(
                           selectInput("dist", 
                                       label = "Select a district",
                                       choices = c("All BC", districts),
                                       selected = "DSI"
                                       ),
                           
                           radioButtons("type", inline = FALSE, 
                                        label = "Choose the type of map",
                                        choices = list("Biogeoclimatic units" = 1, "Species feasibility" = 2),
                                        selected = 1),
                           
                           
                           conditionalPanel(
                             condition = "input.type == 1",
                             selectInput("col1_gcm","Select GCM",choices = gcmOpts),
                             radioButtons("col1_scn","Select Scenario", choices = scenarioOpts)
                             
                           ),
                           
                           conditionalPanel(
                             condition = "input.type ==2",
                             selectInput("sppPick","Select Tree Species",choices = c("Choose one" = "", "")),
                             selectInput("edaPick","Select Site Position",choices = c("C4","B2","D6"),selected = "C4"),
                             radioButtons("feasType","Select map type", choices = c("Feasibility","RawVotes","Change","Loss/Gain"),
                                          selected = "Feasibility")
                           ),
                           
                           
                           radioButtons("time",
                                        label = "Choose a time period",
                                        choices = periodOpts
                           )
                           
                           #switchInput(inputId = "showlegend", value = FALSE, label = "Show map legend")
                    ),
                    style = "opacity: 0.65; z-index: 10 !important;"),
                    
                  leafletOutput(outputId = "map", height = 700) ),
                    #mainPanel(
                      
                      
                      
                      column(4, 
                             selectInput("var1", 
                                         label = "Choose the x-axis variable",
                                         choices = ""))
                    #) 
            
          
           
            
           ),
           tabPanel("About",
                    includeMarkdown("about.Rmd")
           ),
           tabPanel("Model Info",
                    verbatimTextOutput("summary")
           )
)
)