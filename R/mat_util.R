#' @noRd
.is.Matrix <- function(g) "dgCMatrix" %in% class(g) | "dgeMatrix" %in% class(g)

#' @noRd
#' @import igraph
#' @import Matrix
.as.matrix <- function(g)
{
  if (igraph::is_igraph(g)) g <- igraph::get.adjacency(g)
  invisible(as.matrix(g))
}

#' @noRd
.stoch.col.norm <- function(mat) Matrix::as.matrix(.stoch_col_norm_cpp(as.matrix(mat)))
