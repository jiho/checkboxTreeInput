library("shiny")
library("checkboxTreeInput")

tree <- data.frame(id=1:6, parent_id=c(NA, 1, 2, 2, NA, 1))

ui <- function(request) {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("group", label="checkboxGroup selector", choices=1:6, selected=1:3),
        checkboxTreeInput("tree", label="checkboxTree selector", tree=tree, selected=c(1, 6), opened=1),
        bookmarkButton()
       ),
      mainPanel(
        h2("checkboxGroup"),
        verbatimTextOutput("group"),
        h2("checkboxTree"),
        verbatimTextOutput("tree")
      )
    )
  )
}

server <- function(input, output, session) {
  output$group <- renderPrint(input$group)
  output$tree <- renderPrint(input$tree)
}
enableBookmarking(store="url")
shinyApp(ui, server)
