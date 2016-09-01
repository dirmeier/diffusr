#' Find the k-NN nearest neighbors of every node in a graph
#'
#' @export
#' @author Simon Dirmeier
#' @import igraph
#' @import Matrix
#'
#' @param node.idxs  vector of node indexes (1-based) for which the k-NN algorithm is applied
#' @param graph  an <code>igraph</code> object
#' @param k  the depth of the nearest neighbor search
#' @param use.edge.weights  boolean flags if the edge weights should be considered when doing nearest neighbor lookup
#' @param ...  additional params
#' @return  returns the kNN graph as an an <code>igraph</code> object
#' @examples
#' \dontrun{
#'  TODO
#' }
neighbors <- function(node.idxs, graph, k=1, use.edge.weights=F, ...) UseMethod("knn")

#' @noRd
#' @export
#' @import igraph
#' @import Matrix
neighbors.numeric <- function(node.idxs, graph, k=1, use.edge.weights=F, ...)
{
  if (node.idxs) stop("p0 can only contain non-negative values!")
  if (!igraph::is_igraph(graph) & !is.matrix(graph) & !.is.Matrix(graph))
    stop("'graph' is not a graph object or matrix!")
  if (!is.numeric(r)) stop("r has to be numeric!")
  if (!.in(r, 0, 1))  stop("r must be in [0, 1]!")
  if (!is.logical(use.edge.weights)) stop("Use edge weughts should be boolean!")
  .rwr(p0, graph, r)
}

#' @noRd
#' @import igraph
#' @import Matrix
.rwr <-  function(p0, graph, r, use.edge.weights, ...)
{
  mat <- .as.matrix(graph)
  if (any(mat < 0)) stop("graph has to be non-negative")
  if (dim(mat)[1] != dim(mat)[2]) stop("graph has to be of dimension (n x n)!")
  if (dim(mat)[1] != length(p0)) stop("p0 has to have same dim as your graph!")
  mat <- .stoch.col.norm(mat)
  invisible(do_mrwr(p0, mat, r))
}
