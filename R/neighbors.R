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


#' Find the closest neighbors of a group of nodes in a graph.
#'
#' @export
#' @author Simon Dirmeier, email{simon.dirmeier@@bsse.ethz.ch}
#'
#' @import igraph
#'
#' @param nodes  vector of node indexes (1-based) for which the algorithm is applied iteratively
#' @param graph  a non-negative matrix
#' @param k  the depth of the nearest neighbor search
#' @param ...  additional params
#' @return  returns the kNN nodes as integer vector of node indexes
#' @examples
#'  # count of nodes
#'  n <- 10
#'  # indexes (integer) of nodes for which neighbors should be searched
#'  node.idxs <- c(1L, 5L)
#'  # the adjaceny matrix (does not need to be symmetric)
#'  graph <- rbind(cbind(0, diag(n-1)), 0)
#'  # compte the neighbors until depth 3
#'  neighs <- neighbors(node.idxs, graph, 3)
neighbors <- function(nodes, graph, k=1L, ...)
{
  UseMethod("neighbors")
}

#' @export
#' @method neighbors numeric
neighbors.numeric <- function(nodes, graph, k=1L, ...)
{
  if (!is.numeric(nodes) && !is.integer(nodes))
    stop('nodes has to be a vector of integer')
  nodes <- unique(as.integer(nodes))
  if (any(nodes <= 0))
    stop("node idxs have to be 1-indexed!")
  if (!is.matrix(graph))
    stop('please provide a matrix object!')
  if (any(graph < 0))
    stop("graph has to be non-negative")
  if (dim(graph)[1] != dim(graph)[2])
    stop("graph has to be of dimension (n x n)!")
  if ((!is.numeric(k) && !is.integer(k)) || length(k) != 1)
    stop('k has to be a scalar int')
  k <- as.integer(k)
  if (k < 1) stop("k must be greater than 0!")
  invisible(.neighbors.cpp(nodes, graph, k))
}
