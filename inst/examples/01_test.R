tree <- data.frame(id=1:6, parent_id=c(NA, 1, 2, 2, 1, NA))
library(shiny)

ui <- function(request) {
  fluidPage(
    tags$head(tags$style(
"
/* More compact aspect */
.shiny-input-checkboxtree * {
  list-style-type: none;
}
.shiny-input-checkboxtree ul,
.shiny-input-checkboxtree * ul {
  padding-left: 17px;
}
.shiny-input-checkboxtree * .toggle {
  margin-left: -18px; /* this aligns the toggle icons with the boxes */
}
.shiny-input-checkboxtree * .checkbox {
  margin-bottom: 3px;
  margin-top: 3px;
}

/* Display checkboxes directly after toggles */
.shiny-input-checkboxtree * .shiny-options-group {
  display: inline-block;
  margin-bottom: 0;
}

/* Nicely formatted toggle handles */
.shiny-input-checkboxtree * a:hover {
  text-decoration: none;
}
.shiny-input-checkboxtree * .toggle:after {
  font-family: 'Glyphicons Halflings';
  content: '\\e252'
}
.shiny-input-checkboxtree * .toggle.collapsed:after {
  content: '\\e250'
}
"
    )),
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
