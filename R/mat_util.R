# diffusr: network diffusion algorithms in R
#
# Copyright (C) 2016 Simon Dirmeier
#
# This file is part of diffusr.
#
# diffusr is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# diffusr is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with diffusr. If not, see <http://www.gnu.org/licenses/>.

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
.stoch.col.norm <- function(mat)
{
  mat <- as.matrix(mat)
  if (all(.equals.double(colSums(mat), 1, .001)))
  {
    message("not normalizing columns!")
    return(Matrix::as.matrix(mat))
  }
  else
  {
    message("normalizing columns!")
    return(Matrix::as.matrix(.stoch.col.norm.cpp(as.matrix(mat))))
  }
}
