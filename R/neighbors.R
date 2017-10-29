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


#' Graph diffusion using nearest neighbors
#'
#' @description For every node in a set of nodes the graph gets traversed along
#' the node's shortest paths to its neighbors. Nearest neighbors are added
#' until a maximum depth of \code{k} is reached. For settings where there are more
#' than \code{k} neighbors having the same distance, all neighbors are returned.
#'
#' @export
#' @docType methods
#' @rdname nearest-neighbors-methods
#'
#' @param nodes  a \code{n}-dimensional integer vector of node indexes (1-based) for which the algorithm is applied iteratively
#' @param graph  an (\code{n x n})-dimensional numeric non-negative adjacence matrix representing the graph
#' @param k  the depth of the nearest neighbor search, e.g. the depth of the graph traversal
#' @param ...  additional parameters
#' @return  returns the kNN nodes as list of integer vectors of node indexes
#'
#' @examples
#'  # count of nodes
#'  n <- 10
#'  # indexes (integer) of nodes for which neighbors should be searched
#'  node.idxs <- c(1L, 5L)
#'  # the adjaceny matrix (does not need to be symmetric)
#'  graph <- rbind(cbind(0, diag(n-1)), 0)
#'  # compute the neighbors until depth 3
#'  neighs <- nearest.neighbors(node.idxs, graph, 3)
setGeneric(
  "nearest.neighbors",
  function(nodes, graph, k=1L, ...)
  {
    standardGeneric("nearest.neighbors")
  },
  package="diffusr"
)


#' @rdname nearest-neighbors-methods
#' @aliases nearest.neighbors,integer,matrix-method
setMethod(
  "nearest.neighbors",
  signature=signature(nodes="integer", graph="matrix"),
  function(nodes, graph, k=1L, ...)
  {
    if (!is.numeric(nodes) && !is.integer(nodes))
      stop('nodes has to be a vector of integer')
    int.nodes <- unique(as.integer(nodes))
    if (length(int.nodes) != length(nodes))
      warning("casting nodes to int removed some of the indexes.")
    if (any(int.nodes < 1))
      stop("node idxs have to be 1-indexed!")
    if ((!is.numeric(k) && !is.integer(k)) || length(k) != 1 || k < 1)
      stop('k has to be a positive scalar int')
    k <- as.integer(k)
    .check.graph(graph)
    if (any(diag(graph) != 0))
    {
      warning("setting diag of graph to zero")
      diag(graph) <- 0
    }
    l <- neighbors_(int.nodes, graph, k)
    names(l) <- int.nodes
    
    invisible(l)
  }
)
