library("shiny")
library("checkboxTreeInput")

tree <- data.frame(id=1:6, parent_id=c(NA, 1, 2, 2, 1, NA))

ui <- function(request) {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        checkboxTreeInput("taxo", label="Tree selector", tree=tree, selected=c(1, 6), opened=1)
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
