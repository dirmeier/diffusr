#' Do a Markov random walk on a graph
#'
#' @export
#' @author Simon Dirmeier
#'
#' @param p0  the staring distribution for the Markov chain
#' @param graph  a weighted <code>igraph</code> object
#' @param r  the restart probability if a Markov random walk with restart is desired
#' @param ...  additional params
#' @return  returns the stationary distribution of the
#' @examples
#' \dontrun{
#'  TODO
#' }
random.walk <- function(p0, graph, r, ...) UseMethod("random.walk")


#' @noRd
#' @export
#' @import igraph
#' @import Matrix
random.walk.numeric <- function(p0, graph, r, ...)
{
  if (any(p0 < 0)) stop("p0 can only contain non-negative values")
  if (!.equals.double(p0, 1, .0001)) stop("p0 does not sum to 1.")
  if (class())
}

