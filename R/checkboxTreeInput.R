#' Convert a tree data.frame into a ul list
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label Display label for the control, or \code{NULL} for no label.
#' @param tree data.frame defining a tree, with columns \code{id} and \code{parent_id} at least.
#' @param selected The nodes that should be initially selected, if any. All selected nodes (and their ancestors) will also be opened, to be visible.
#' @param opened The nodes that should be initially opened, if any. All ancestor nodes will also be opened.
#'
#' @return A list of HTML elements that can be added to a UI definition.
#'
#' @export
#' @examples
#' tree <- data.frame(id=1:6, parent_id=c(NA, 1, 2, 2, 1, NA))
#' checkboxTreeInput("foo", "bar", tree=tree)
#' checkboxTreeInput("foo", "bar", tree=tree, selected=1)
#' checkboxTreeInput("foo", "bar", tree=tree, )
checkboxTreeInput <- function(inputId, label=NULL, tree, selected=NULL, opened=NULL) {
  # TODO handle restoreInput based on how it looks in checkboxGroupInput

  # check input
  wrong_ids <- setdiff(selected, tree$id)
  if (length(wrong_ids) > 0) {
    warning("The following elements cannot be selected because they do not match a node id in the tree:\n  ", paste(wrong_ids, collapse=", "))
  }
  wrong_ids <- setdiff(opened, tree$id)
  if (length(wrong_ids) > 0) {
    warning("The following elements cannot be opened because they do not match a node id in the tree:\n  ", paste(wrong_ids, collapse=", "))
  }

  # mark selected items in a new column
  if (! "selected" %in% names(tree)) {
    tree$selected <- FALSE
  }
  tree$selected[tree$id %in% selected] <- TRUE

  # mark opened items in a new column
  if (! "opened" %in% names(tree)) {
    tree$opened <- FALSE
  }
  # open all parents of selected or opened nodes
  to_open <- c(tree$id[tree$selected], opened)
  if (length(to_open) > 0 ) {
    to_open <- c(ancestors(to_open, tree), to_open)
    tree$opened[tree$id %in% to_open] <- TRUE
  }

  # create a name column from ids if it does not exists
  if (! "name" %in% names(tree)) {
    tree$name <- tree$id
  }

  # Create the output list
  list(
    shiny::singleton(shiny::tags$head(
      initResourcePath(),
      shiny::tags$link(rel="stylesheet", type="text/css", href="checkboxTreeInput.css")
    )),
    shiny::tags$div(id=inputId, class="shiny-input-checkboxtree shiny-input-checkboxgroup shiny-input-container",
      shiny:::controlLabel(inputId, label),
      shiny::tags$ul(
        # iterate from the roots
        lapply(roots(tree), tree_li, tree=tree, inputId=inputId)
      )
    )
  )
}

# Create a checkboxInput
# @param x a line of \code{tree}
# @param inputId
treeCheckboxInput <- function(x, inputId) {
  choices <- x$id
  names(choices) <- x$name
  if (x$selected) {
    selected <- x$id
  } else {
    selected <- NULL
  }
  shiny:::generateOptions(inputId, choices=choices, selected=selected, inline=FALSE, type="checkbox")
}

# Create a <li> item, with a nested, collapsable <ul> item if necessary
# @param is one node id, from \code{tree$i}
# @param tree
# @param inputId
tree_li <- function(id, tree, inputId) {
  childs <- children(id, tree, n=1)
  x <- tree[tree$id == id,]
  if (length(childs)>0) {
    out <- shiny::tags$li(
      shiny::tags$a(`data-toggle`="collapse", href=paste0("#checkboxTreeCollapse_", id), class=paste("toggle", if(!x$opened) {"collapsed"})),
      treeCheckboxInput(x, inputId),
      shiny::tags$ul(class=paste("collapse", if(x$opened) {"in"}), id=paste0("checkboxTreeCollapse_", id),
        lapply(childs, tree_li, tree=tree, inputId=inputId)
      )
    )
  } else {
    out <- shiny::tags$li(
      treeCheckboxInput(x, inputId)
    )
  }
  return(out)
}
