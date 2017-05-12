#' Coerce to a Tree Data Frame (treedf)
#'
#' Functions to check if an object is a \code{treedf}, or coerce it if possible.
#'
#' @param x An R object.
#'
#' @details A \code{treedf} is a \code{data.frame} in which each line defines a node in a tree and is characterised by columns \describe{
#' \item{id}{the unique identifier of the node,}
#' \item{parent_id}{the identifier of the parent of the node,}
#' \item{name}{the (displayed) name of the node (optional),}
#' \item{selected}{logical, if TRUE, the node will be selected in \code{\link{checkboxTreeInput}} (optional),}
#' \item{opened}{logical, if TRUE, the node will be opened in \code{\link{checkboxTreeInput}} (optional).}
#' }
#' The structure of the tree is therefore defined by the relationship between \code{id} and \code{parent_id}.
#'
#' Conversion functions are provided for various tree structures available in R. If the structure of the tree is simply defined by different columns of a data.frame, one per level (e.g. organisation, department, team or family, genus, species), join them in \code{pathString} and use this argument to convert the data.frame into a treedf (see examples).
#'
#' @export
#' @aliases treedf
#' @examples
#' # definition of a tree through id and parent_id
#' x <- data.frame(id=c(1, 2, 3, 4), parent_id=c(NA, 1, 2, 1))
#' ui <- fluidPage(checkboxTreeInput(inputId="tree", tree=x))
#' server <- function(input, output) { }
#' if (interactive()) shinyApp(ui, server)
#'
#' # definition of a tree by levels
#' x <- data.frame(
#'        organisation=rep(c("R", "MATLAB"), each=3),
#'        department=c("code", "", "doc", "code", "doc", "marketing"),
#'        team=c("core", "", "", "central", "help", "")
#'      )
#' x$pathString <- paste(x$organisation, x$department, x$team, sep="/")
#' x$pathString <- gsub("/*$", "", x$pathString)
#' x <- as.treedf(x)
#' ui <- fluidPage(checkboxTreeInput(inputId="tree", tree=x))
#' if (interactive()) shinyApp(ui, server)
as.treedf <- function(x, ...) {
  UseMethod("as.treedf", x)
}

#' @param pathName The name of the column containing the path of the current node. If this column exists in \code{x}, then it defines the structure of the tree, the data.frame is converted into a tree through \code{\link[data.tree]{as.Node.data.frame}}, and the result is turned into a \code{treedf}. If the column does not exist, then \code{x} is supposed to already have the structure of a \code{treedf}.
#' @param pathDelimiter The delimiter used to separate nodes in \code{pathName}.
#' @param ... Passed to \code{\link[data.tree]{as.Node.data.frame}} when used with a data.frame that contains \code{pathName}; ignored otherwise.
#'
#' @rdname as.treedf
#' @export
as.treedf.data.frame <- function(x, pathName="pathString", pathDelimiter="/", ...) {
  if (pathName %in% names(x)) {
    # add a root (required for data.tree)
    x[[pathName]] <- paste("###", x[[pathName]], sep=pathDelimiter)
    # convert into data.tree then into treedf
    x <- data.tree::as.Node(x, pathName=pathName, pathDelimiter=pathDelimiter, ...)
    x <- as.treedf.Node(x)
    # remove the added root
    root_id <- roots(x)
    x <- x[x$id != root_id,]
    # and branch children to the root
    x$parent_id[x$parent_id == root_id] <- NA
  } else {
    check_treedf(x)
    class(x) <- c("treedf", class(x))
  }
  
  # remove row.names for cleanness
  row.names(x) <- NULL
  return(x)
}

#' @rdname as.treedf
#' @export
as.treedf.Node <- function(x, ...) {
  # extract all relevant elements for a treedf
  d <- data.frame(
    id=x$Get("pathString"),
    # TODO handle if id attribute is present
    parent_id=x$Get(function(x) {ifelse(is.null(x$parent), NA, x$parent$pathString)}),
    name=x$Get("name"),
    selected=x$Get("selected"),
    opened=x$Get("opened"),
    pathString=str_replace(x$Get("pathString"), "^###\\/", "")
  )
  # make ids numeric to speed things up
  d$id <- factor(d$id)
  d$parent_id <- factor(d$parent_id, levels=levels(d$id))
  d$id <- as.numeric(d$id)
  d$parent_id <- as.numeric(d$parent_id)
  # remove all NA columns (i.e. empty attributes)
  d <- d[,!sapply(d, function(x) {all(is.na(x))})]

  class(d) <- c("treedf", class(d))

  # remove row.names for cleanness
  row.names(d) <- NULL
  return(d)
}

#' @rdname as.treedf
#' @export
is.treedf <- function(x) {
  inherits(x, "treedf")
}
