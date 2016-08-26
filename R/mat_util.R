#' @noRd
.is.Matrix <- function(g) "dgCMatrix" %in% class(g)

#' @noRd
#' @import igraph
#' @import Matrix
.as.Matrix <- function(g)
{
  if (igraph::is_igraph(g)) g <- igraph::get.adjacency(g)
  invisible(Matrix::Matrix(g))
}
