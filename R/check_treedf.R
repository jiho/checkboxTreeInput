# Check the consistency of a treedf object
check_treedf <- function(x) {
  if (! all(c("id", "parent_id") %in% names(x))) {
    stop("A treedf should have columns `id` and `parent_id`.")
  }
  if (any(duplicated(x$id))) {
    stop("Column `id` should contain unique identifiers. ", paste0(x$id[duplicated(x$id)], collapse=", "), " are duplicated.")
  }
  if ("selected" %in% names(x)) {
    if (! is.logical(x$selected)) {
      stop("Column `selected` should be logical (TRUE/FALSE)")
    }
  }
  if ("opened" %in% names(x)) {
    if (! is.logical(x$opened)) {
      stop("Column `opened` should be logical (TRUE/FALSE)")
    }
  }
  return(invisible(TRUE))
}
