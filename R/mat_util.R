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

#' @noRd
.stoch.col.norm <- function(mat) Matrix::as.matrix(do_stoch_col_norm(mat))
