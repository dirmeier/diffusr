#' Finds the k-NN nearest neighbors of every node in a graph
#'
#' @export
#' @author Simon Dirmeier
#' @import igraph
#' @import Matrix
#'
#' @param nodes  vector of nodes for which the k-NN algorithm is applied  is applied
#' @param graph  an <code>igraph</code> object
#' @param r  the restart probability if a Markov random walk with restart is desired
#' @return the kNN graph as an an <code>igraph</code> object
#' @examples
#' \dontrun{
#'  TODO
#' }
random.walk <- function(nodes, graph, r, ...) UseMethod("random.walk")


#' @noRd
#' @export
random.walk.default <- function(genes, graph, r, ...)
{
  # TODO
}
