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
#' @author Simon Dirmeier, email{simon.dirmeier@@bsse.ethz.ch}
#'
#' @import igraph
#'
#' @param v  the starting distribution of the Markov chain
#' @param graph  a non-negative matrix
#' @param t  the restart probability if a Markov random walk with restart is desired
#' @param ...  additional params
#' @return  returns the heat on every node as an
heat.diffusion <- function(v, graph, t=100, ...)
{
  UseMethod("heat.diffusion")
}

#' @export
#' @method heat.diffusion numeric
heat.diffusion.numeric <- function(v, graph, t=100, ...)
{

}


