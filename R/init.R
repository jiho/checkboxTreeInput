.global <- new.env()

initResourcePath <- function() {
  if (is.null(.global$loaded)) {
    shiny::addResourcePath(prefix="checkboxTreeInput", directoryPath=system.file("www", package="checkboxTreeInput"))
    .global$loaded <- TRUE
  }
  shiny::HTML("")
}
