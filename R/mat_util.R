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

#' Create a stochastically normalized matrix/vector
#'
#' @export
#' @author Simon Dirmeier, \email{simon.dirmeier@@gmx.de}
#'
#' @param obj  matrix/vector that is stochstically normalized
#' @param ...  additional params
#' @return  returns the normalized matrix/vector
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
  if (any(obj < 0.0))
    stop('please provide an object with only non-negative values!')
  if (is.matrix(obj))
  {
    if (nrow(obj) != ncol(obj))
      stop('please provide a square matrix!')
    if (!all(.equals.double(colSums(obj), 1, .001)))
    {
      message("normalizing column vectors!")
      obj <- .stoch.col.norm.cpp(obj)
    }
  }
  else if (is.vector(obj))
  {
    if (!.equals.double(sum(obj), 1, .001))
    {
      message("normalizing vector!")
      obj <- obj/sum(obj)
    }
  }
  return(obj)
}

#' Calculate the Laplacian of a matrix
#'
#' @export
#' @author Simon Dirmeier, \email{simon.dirmeier@@gmx.de}
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
