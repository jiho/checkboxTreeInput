ui <- function(request) {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("taxo", label="Tree selector", choices=c("foo", "bar", "bob"), selected=c("foo", "bar"), width="100%")
      ),
      mainPanel(
        verbatimTextOutput("selected")
      )
    )
  )
}

server <- function(input, output, session) {
  output$selected <- renderPrint(
    input$taxo
  )
}

shinyApp(ui, server)
