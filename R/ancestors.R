#' Get the ancestors of a node
#'
#' @param id numeric ids of nodes.
#' @param tree a \code{\link{treedf}} data.frame, with columns \code{id} and \code{parent_id} at least.
#' @param n number of levels to look up; n=1 gives the parent, n=2 gives the grand-parent, etc.
#'
#' @return A vector of ids of the ancestors of the focus node(s).
#'
#' @export
#' @examples
#' tree <- data.frame(id=c(1, 2, 3, 4), parent_id=c(NA, 1, 2, 2))
#' ancestors(3, tree)
#' ancestors(3, tree, n=1)
#' ancestors(c(1,4), tree)
ancestors <- function(id, tree, n=Inf) {
  # checks
  all_ids <- c(tree$id, tree$parent_id)
  missing_ids <- setdiff(id, all_ids)
  if ( length(missing_ids) > 0 ) {
    stop("Some ids could not be found: ", paste0(missing_ids, collapse=", "))
  }
  # initialise
  ancestors <- c()
  parent <- id
  count <- -1
  while ( !all(is.na(parent)) & count < n) {
    # add the ancestors
    ancestors <- c(ancestors, parent)
    # the new parents are the parents of the current parents (ouch...)
    parent <- tree$parent_id[tree$id %in% parent]
    count <- count + 1
  }
  # remove the original ids
  ancestors <- ancestors[-c(1, length(id))]
  # remove NAs (roots) and duplicates
  ancestors <- unique(stats::na.omit(ancestors))
  return(ancestors)
}
