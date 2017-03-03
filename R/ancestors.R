#' Get the ancestors of a node
#'
#' @param id numeric ids of nodes
#' @param tree tree data.frame, with columns `id` and `parent_id` at least
#' @param n number of levels to look up; n=1 gives the parent, n=2 gives the grand-parent, etc.
#'
#' @return A vector of ids of the focus node and its ancestors.
#'
#' @export
#'
#' @examples
#' tree <- data.frame(id=c(1, 2, 3, 4), parent_id=c(NA, 1, 2, 2))
#' ancestors(3, tree)
#' ancestors(3, tree, n=1)
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
    ancestors <- c(parent, ancestors)
    # the new parents are the parents of the current parents (ouch...)
    parent <- tree$parent_id[which(tree$id %in% parent)]
    count <- count + 1
  }
  return(unique(ancestors))
}

#' Get the children of a node
#'
#' @inheritParams ancestors
#' @param n number of levels to look down; n=1 gives the direct children, n=2 gives grand children (i.e. children of all children), etc.
#'
#' @return A vector of ids of the focus node and its children.
#'
#' @export
#'
#' @examples
#' tree <- data.frame(id=c(1, 2, 3, 4), parent_id=c(NA, 1, 2, 2))
#' children(1, tree)
#' children(1, tree, n=1)
children <- function(id, tree, n=Inf) {
  # invert parents and children
  idx <- match(c("id", "parent_id"), names(tree))
  names(tree)[idx] <- c("parent_id", "id")
  # and get ancestors... which are now children
  rev(ancestors(id=id, tree=tree, n=n))
}
