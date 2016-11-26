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

#' Graph diffusion using a Markov random walk
#'
#' @description A Markov Random Walk takes an inital distribution \code{p0} and calculates the stationary distribution of that.
#' The diffusion process is regulated by a restart probability \code{r}.
#' It basically controls how often the MRW jumps back to the initial values.
#'
#' @export
#' @author Simon Dirmeier, \email{simon.dirmeier@@gmx.de}
#'
#' @param p0  the starting distribution of the Markov chain
#' @param graph  a non-negative matrix
#' @param r  the restart probability if a Markov random walk with restart is desired
#' @param ...  additional params
#' @return  returns the stationary distribution as vector
#'
#' @references
#' Tong, H., Faloutsos, C., & Pan, J. Y. (2006),
#' Fast random walk with restart and its applications.\cr \cr
#' Köhler, S., Bauer, S., Horn, D., & Robinson, P. N. (2008),
#' Walking the interactome for prioritization of candidate disease genes.
#' \emph{The American Journal of Human Genetics}\cr \cr
#'
#' @examples
#' # count of nodes
#' n <- 5
#' # starting distribution (has to sum to one)
#' p0    <- as.vector(rmultinom(1, 1, prob=rep(.2, n)))
#' # adjacency matrix (either normalized or not)
#' graph <- matrix(abs(rnorm(n*n)), n, n)
#' # computation of stationary distribution
#' pt    <- random.walk(p0, graph)
random.walk <- function(p0, graph, r=.5, ...)
{
  UseMethod("random.walk")
}

#' @export
#' @method random.walk numeric
random.walk.numeric <- function(p0, graph, r=.5, ...)
{
  .check.restart(r)
  .check.vector(p0)
  .check.graph(graph, p0)
  invisible(.mrwr.cpp(normalize(p0), normalize(graph), r))
}
