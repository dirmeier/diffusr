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
#' @import Matrix
#'
#' @param node.idxs  vector of node indexes (1-based) for which the algorithm is applied iteratively
#' @param graph  an \code{igraph} object
#' @param k  the depth of the nearest neighbor search
#' @param use.edge.weights  boolean flags if the edge weights should be considered when doing nearest neighbor lookup
#' @param ...  additional params
#' @return  returns the kNN graph as an an \code{igraph} object
#' @examples
#'  # count of nodes
#'  n <- 10
#'  # indexes (integer) of nodes for which neighbors should be searched
#'  node.idxs <- c(1L, 5L)
#'  # the adjaceny matrix (does not need to be symmetric)
#'  graph <- rbind(cbind(0, diag(n-1)), 0)
#'  # compte the neighbors until depth 3
#'  neighs <- neighbors(node.idxs, graph, 3)
neighbors <- function(node.idxs, graph, k=1L, use.edge.weights=F, ...)
{
  UseMethod("neighbors")
}

#' @noRd
#' @export
#' @import igraph
#' @import Matrix
neighbors.integer <- function(node.idxs, graph, k=1L, use.edge.weights=F, ...)
{
  if (any(node.idxs <= 0)) stop("Node idxs have to be 1-indexed!")
  if (!igraph::is_igraph(graph) & !is.matrix(graph) & !.is.Matrix(graph))
    stop("'graph' is not a graph object or matrix!")
  if (!is.numeric(k)) stop("k is not numeric!")
  if (!is.integer(k)) {
    k <- as.integer(k)
    message("Casting k to int!")
  }
  if (k < 1) stop("k must be greater than 0!")
  if (!is.logical(use.edge.weights))
    stop("use.edge.weights should be boolean!")
  if (use.edge.weights) stop("Not yet implemented!")
  .neighbors(node.idxs, graph, k, use.edge.weights)
}

#' @noRd
#' @import igraph
#' @import Matrix
.neighbors <-  function(node.idxs, graph, k, use.edge.weights)
{
  mat <- .as.matrix(graph)
  if (any(mat < 0)) stop("graph has to be non-negative")
  if (dim(mat)[1] != dim(mat)[2]) stop("graph has to be of dimension (n x n)!")
  invisible(.neighbors.cpp(as.integer(node.idxs), graph,
                           as.integer(k), use.edge.weights))
}

