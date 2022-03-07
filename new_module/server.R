server <- function(input, output, session) {
  observeEvent(input$showSidebar, {
    shinyjs::toggle(id = "Sidebar")
  }) 
}