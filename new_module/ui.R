ui <- tagList(
  
  
  shinyjs::useShinyjs(),
  includeCSS("www/style.css"),
  
  navbarPage(
           
           title = div(img(src="logo.svg"), "The CCISS Tool"),
           #theme = shinytheme("united"),
          
           tabPanel("App",
                    
                  #div(id = 'Sidebar',
                  #sidebarPanel(
                  column(8,
                  absolutePanel(top = 60, left = 5, right = "auto", bottom = "auto",
                                width = 300, height = "auto",draggable = TRUE,
                    
                   wellPanel(
                    #        selectInput("dist", 
                    #                    label = "Select a district",
                    #                    choices = c("All BC", districts),
                    #                    selected = "All BC"
                    #                    ),
                           
                           radioButtons("type", inline = FALSE, 
                                        label = "Choose the type of map",
                                        choices = list("Biogeoclimatic units" = 1, "Species feasibility" = 2),
                                        selected = 1),
                           
                           sliderInput("opacity", label = "Opacity control", min = 0.2, max = 1, value = 0.8),
                           
                           conditionalPanel(
                             condition = "input.type == 1",
                             selectInput("col1_gcm","Select GCM",choices = gcmOpts),
                             radioButtons("col1_scn","Select Scenario", choices = scenarioOpts)
                             
                           ),
                           
                           conditionalPanel(
                             condition = "input.type == 2",
                             selectInput("sppPick","Select Tree Species",choices = spp$species, selected = "Fd"),
                             selectInput("edaPick","Select Site Position",choices = c("B2","C4","E6"),selected = "E6"),
                             radioButtons("feasType","Select map type", choices = c("Feasibility","RawVotes","Change"),
                                          selected = "Feasibility")
                           ),
                           
                           
                           radioButtons("time",
                                        label = "Choose a time period",
                                        choices = periodOpts
                           )
                           
                           #switchInput(inputId = "showlegend", value = FALSE, label = "Show map legend")
                    ),
                    style = "opacity: 0.65; z-index: 10 !important;"),
                    
                  leafletOutput(outputId = "map", height = 700),
                  htmlOutput("zoomlevel_display"),
                  
                  #DT::dataTableOutput("test_tb")
                  ),
                    
                      
                      
                      
                      column(4, 
                             
                             selectInput("subarea", 
                                         label = "Choose a subregion",
                                         choices = c("None",districts)),
                             
                             uploadFileUI("uploadfile"),
                             
                             br(),
                             
                             tabsetPanel(
                               
                               tabPanel("Graphic summary",
                                        
                                        fluidRow(
                                        column(4,
                                        selectInput("var1", 
                                                     label = "Choose the x-axis variable",
                                                     choices = climvars,
                                                     selected = "MAT")),
                                        column(4,
                                        conditionalPanel(
                                          condition = "input.type == 1",
                                          selectizeInput("subzone","Select a BGC subzone",choices = "", multiple = TRUE)
                                        ))),
                                        
                                        plotlyOutput("scatterplot")%>%withSpinner()
                                        ),
                               
                               tabPanel("Climate variables",
                                        
                                        fluidRow(
                                        column(4,
                                        selectInput("climvar1", 
                                                    label = "Choose the x-axis variable",
                                                    choices = climvars,
                                                    selected = "MAT")),
                                        column(4,
                                        selectInput("climvar2", 
                                                           label = "Choose the y-axis variable",
                                                           choices = climvars,
                                                           selected = "MAT"))
                                        ),
                                        plotlyOutput("climatevarplot")%>%withSpinner())
                             )
                             
                             
                             
                             

                      )
                   
            
          
           
            
           ),
           tabPanel("About",
                    includeMarkdown("about.Rmd")
           ),
           tabPanel("Model Info",
                    verbatimTextOutput("summary")
           )
)
)