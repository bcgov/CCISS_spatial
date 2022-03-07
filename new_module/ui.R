ui <- tagList(
  
  
  shinyjs::useShinyjs(),
  includeCSS("www/style.css"),
  
  navbarPage(
           
           title = div(img(src="logo.svg"), "The CCISS Tool"),
           theme = shinytheme("united"),
          
           tabPanel("App",
                    
                  div(id = 'Sidebar',
                  sidebarPanel(
                           
                           selectInput("dist", 
                                       label = "Select a district",
                                       choices = districts$district),
                           
                           radioButtons("type", inline = FALSE, 
                                        label = "Choose the type of map",
                                        choices = list("Climate variables" = 1, "Biogeoclimatic units" = 2, "Species feasibility" = 3),
                                        selected = 2),
                           
                           radioButtons("time",
                                        label = "Choose a time period",
                                        choices = periodOpts$futureperiod
                           ) 
                    )),
                    
                    
                    mainPanel(
                      
                      column(5,
                             actionButton("showSidebar", "Show sidebar"),
                             leafletOutput(outputId = "map") ),
                      
                      column(5, 
                             selectInput("var1", 
                                         label = "Choose the x-axis variable",
                                         choices = ""))
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