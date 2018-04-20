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
#'
#' @param obj  matrix/vector that is stochstically normalized
#' @param ...  additional params
#' @return  returns the normalized matrix/vector
#' @examples
#'  W <- matrix(abs(rnorm(10000)), 100, 100)
#'  stoch.W <- normalize.stochastic(W)
normalize.stochastic <- function(obj, ...)
{
  UseMethod("normalize.stochastic")
}


#' @export
#' @method normalize.stochastic numeric
normalize.stochastic.numeric <- function(obj, ...)
{
  if (any(obj < 0.0))
    stop('please provide an object with only non-negative values!')
  if (is.matrix(obj))
  {
    if (!all(.equals.double(colSums(obj), 1, .001)))
    {
      message("normalizing column vectors!")
      obj <- stoch_col_norm_(obj)
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
#'
#' @param obj  matrix for which the Laplacian is calculated
#' @param ...  additional params
#' @return  returns the Laplacian
#' @examples
#'  W <- matrix(abs(rnorm(10000)), 100, 100)
#'  lapl.W <- normalize.laplacian(W)
normalize.laplacian <- function(obj, ...)
{
  UseMethod("normalize.laplacian")
}


#' @export
#' @method normalize.laplacian numeric
normalize.laplacian.numeric <- function(obj, ...)
{
  if (!is.matrix(obj)) stop('please provide a matrix object!')
  if (nrow(obj) != ncol(obj)) stop('please provide a square matrix!')
  if (any(obj < 0.0))
    stop('please provide a matrix with only non-negative alues!')
  lapl <- laplacian_(obj)

  lapl
}



#' Correct for hubs in an adjacency matrix
#'
#' @export
#'
#' @param obj  matrix for which hubs are corrected
#' @return  returns the matrix with hub correction
#' @examples
#'  W <- matrix(abs(rnorm(10000)), 100, 100)
#'  cor.hub <- hub.correction(W)
hub.correction <- function(obj)
{
  UseMethod("hub.correction")
}


#' @export
#' @method hub.correction numeric
hub.correction.numeric <- function(obj)
{
  if (!is.matrix(obj)) stop('please provide a matrix object!')
  if (nrow(obj) != ncol(obj)) stop('please provide a square matrix!')
  if (any(obj < 0.0))
    stop('please provide a matrix with only non-negative alues!')
  message("Correcting for hub degrees.")
  hub.mat <- hub_normalize_(obj)

  hub.mat
}


#' @noRd
#' @importFrom igraph graph_from_adjacency_matrix components
.is.ergodic <- function(obj)
{
  adj   <- igraph::graph_from_adjacency_matrix(
    obj, mode="directed", weighted=TRUE)
  comps <- igraph::components(adj)
  ifelse(length(comps$csize) == 1, TRUE, FALSE)
}
