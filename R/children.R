#' Get the children of a node
#'
#' @inheritParams ancestors
#' @param n number of levels to look down; n=1 gives the direct children, n=2 gives grand children (i.e. children of all children), etc.
#'
#' @return A vector of ids of the children of the focus node(s).
#'
#' @export
#' @examples
#' tree <- data.frame(id=c(1, 2, 3, 4), parent_id=c(NA, 1, 2, 2))
#' children(1, tree)
#' children(1, tree, n=1)
#' children(c(2,3), tree)
children <- function(id, tree, n=Inf) {
  # invert parents and children
  idx <- match(c("id", "parent_id"), names(tree))
  names(tree)[idx] <- c("parent_id", "id")
  # and get ancestors... which are now children
  ancestors(id=id, tree=tree, n=n)
}
