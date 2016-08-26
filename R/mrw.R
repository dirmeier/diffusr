#' Do a Markov random walk on a graph
#'
#' @export
#' @author Simon Dirmeier
#' @import igraph
#' @import Matrix
#'
#' @param p0  the staring distribution for the Markov chain
#' @param graph  a weighted <code>igraph</code> object
#' @param r  the restart probability if a Markov random walk with restart is desired
#' @return  returns the stationary distribution of the
#' @examples
#' \dontrun{
#'  TODO
#' }
random.walk <- function(p0, graph, r, ...) UseMethod("random.walk")


#' @noRd
#' @export
random.walk.default <- function(p0, graph, r, ...)
{
  # TODO
}
