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

#' Do graph diffusion using an insulated heat kernel.
#'
#' @export
#' @author Simon Dirmeier, \email{simon.dirmeier@@gmx.de}
#'
#' @param h  the starting heat distribution
#' @param graph  a non-negative matrix
#' @param r 'rate' of heat diffusion, where 1 is the maximum diffusion and 0 no diffusion at al
#' @param ...  additional params
#' @return  returns the heat on every node as a vector
#' @examples
#' # count of nodes
#' n <- 5
#' # starting distribution (has to sum to one)
#' h    <- rmultinom(1, 1, prob=rep(.2, n))
#' # adjacency matrix (either normalized or not)
#' graph <- matrix(abs(rnorm(n*n)), n, n)
#' # computation of stationary distribution
#' pt    <- heat.diffusion(h, graph)
heat.diffusion <- function(h, graph, r=.5, ...)
{
  UseMethod("heat.diffusion")
}

#' @export
#' @method heat.diffusion numeric
heat.diffusion.numeric <- function(h, graph, r=.5, ...)
{
  if (any(h < 0))
    stop("h can only contain non-negative values!")
  if (!is.vector(h))
    stop('vectorial h required')
  if (!is.numeric(r))
    stop("r has to be numeric!")
  if (!.in(r, 0, 1))
    stop("r must be in [0, 1]!")
  if (!is.matrix(graph))
    stop('please provide a matrix object!')
  if (any(graph < 0))
    stop("graph has to be non-negative")
  if (dim(graph)[1] != dim(graph)[2])
    stop("graph has to be of dimension (n x n)!")
  if (dim(graph)[1] != length(h))
    stop("h has to have same dimension as your graph!")
  invisible(.heat.diffusion.cpp(h, normalize(graph), 1 - r))
}


