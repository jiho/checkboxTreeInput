# Extract the ids of the root(s) of a treedf (=items with no parent)
roots <- function(tree) {
  idx <- which(is.na(tree$parent_id))
  return(tree$id[idx])
}
