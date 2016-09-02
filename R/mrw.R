#' Do a Markov random walk on a graph.
#'
#' @export
#' @author Simon Dirmeier, email{simon.dirmeier@@bsse.ethz.ch}
#'
#' @import igraph
#' @import Matrix
#'
#' @param p0  the starting distribution of the Markov chain
#' @param graph  a weighted \code{igraph} object
#' @param r  the restart probability if a Markov random walk with restart is desired
#' @param ...  additional params
#' @return  returns the stationary distribution of the Markov random walk
#' @examples
#' # count of nodes
#' n <- 5
#' # starting distribution (has to sum to one)
#' p0    <- rmultinom(1, 1, prob=rep(.2, n))
#' # adjacency matrix (either normalized or not)
#' graph <- matrix(abs(rnorm(n*n)), n, n)
#' # computation of stationary distribution
#' pt    <- random.walk(p0, graph)
random.walk <- function(p0, graph, r=.5, ...) UseMethod("random.walk")

#' @noRd
#' @export
#' @import igraph
#' @import Matrix
random.walk.numeric <- function(p0, graph, r=.5, ...)
{
  if (any(p0 < 0)) stop("p0 can only contain non-negative values!")
  if (!.equals.double(sum(p0), 1, .0001)) stop("p0 does not sum to 1!")
  if (!igraph::is_igraph(graph) & !is.matrix(graph) & !.is.Matrix(graph))
    stop("'graph' is not a graph object or matrix!")
  if (!is.numeric(r)) stop("r has to be numeric!")
  if (!.in(r, 0, 1))  stop("r must be in [0, 1]!")
  .rwr(p0, graph, r)
}

#' @noRd
#' @import igraph
#' @import Matrix
.rwr <-  function(p0, graph, r)
{
  mat <- .as.matrix(graph)
  if (any(mat < 0)) stop("graph has to be non-negative")
  if (dim(mat)[1] != dim(mat)[2]) stop("graph has to be of dimension (n x n)!")
  if (dim(mat)[1] != length(p0)) stop("p0 has to have same dim as your graph!")
  mat <- .stoch.col.norm(mat)
  invisible(.mrwr_cpp(p0, mat, r))
}



