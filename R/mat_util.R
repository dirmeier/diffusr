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

#' Calculate a stochastic column normalized matrix
#'
#' @export
#' @author Simon Dirmeier, email{simon.dirmeier@@bsse.ethz.ch}
#'
#' @param obj  matrix for which the normalized stochastic matrix is created
#' @param ...  additional params
#' @return  returns the normalized matrix
#' @examples
#' W <- matrix(abs(rnorm(10000)), 100, 100)
#' stoch.W <- normalize(W)
normalize <- function(obj, ...)
{
  UseMethod("normalize")
}

#' @export
#' @method normalize numeric
normalize.numeric <- function(obj, ...)
{
  if (!is.matrix(obj)) stop('please provide a matrix object!')
  if (nrow(obj) != ncol(obj)) stop('please provide a square matrix!')
  if (any(obj < 0.0))
    stop('please provide a matrix with only non-negative alues!')
  if (!all(.equals.double(colSums(obj), 1, .001)))
  {
    message("normalizing columns!")
    obj <- .stoch.col.norm.cpp(obj)
  }
  return(obj)
}

#' Calculate the Laplacian of a matrix
#'
#' @export
#' @author Simon Dirmeier, email{simon.dirmeier@@bsse.ethz.ch}
#'
#' @param obj  matrix for which the Laplacian is calculated
#' @param ...  additional params
#' @return  returns the Laplacian
#' @examples
#' W <- matrix(abs(rnorm(10000)), 100, 100)
#' lapl.W <- laplacian(W)
laplacian <- function(obj, ...)
{
  UseMethod("laplacian")
}

#' @export
#' @method laplacian numeric
laplacian.numeric <- function(obj, ...)
{
  if (!is.matrix(obj)) stop('please provide a matrix object!')
  if (nrow(obj) != ncol(obj)) stop('please provide a square matrix!')
  if (any(obj < 0.0))
    stop('please provide a matrix with only non-negative alues!')
  lapl <- .laplacian.cpp(obj)
  return(lapl)
}
