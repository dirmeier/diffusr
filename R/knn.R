#' Find the k-NN nearest neighbors of every node in a graph
#'
#' @export
#' @author Simon Dirmeier
#' @import igraph
#' @import Matrix
#'
#' @param nodes  vector of nodes for which the k-NN algorithm is applied  is applied
#' @param graph  an <code>igraph</code> object
#' @param k  the depth of the nearest neighbor search
#' @return  returns the kNN graph as an an <code>igraph</code> object
#' @examples
#' \dontrun{
#'  TODO
#' }
knn <- function(nodes, graph, k=1, ...) UseMethod("knn")


#' @noRd
#' @export
knn.default <- function(nodes, graph, k=1,...)
{
  # TODO
}
