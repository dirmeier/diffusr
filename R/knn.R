#' Finds the k-NN nearest neighbors of every node in a graph
#'
#' @export
#' @author Simon Dirmeier
#' @import igraph
#' @import Matrix
#' @param  nodes vector of nodes for which the k-NN algorithm is applied  is applied
knn <- function(nodes, graph, k=1, ...) UseMethod("knn")


#' @noRd
#' @export
knn.default <- function(genes, graph, k=1,...)
{
  # TODO
}
