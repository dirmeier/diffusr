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
.equals.double <- function(val, cmp, delta)
{
  val  <= cmp + delta  &  val >=  cmp - delta
}

#' @noRd
.in <- function(val, lower, upper)
{
  val >= lower & val <= upper
}

#' @noRd
.check.restart <- function(s)
{
  name <- deparse(substitute(s))
  if (!is.numeric(s))
    stop(paste(name, "has to be numeric!"))
  if (!.in(s, 0, 1))
    stop(paste(name, "must be in [0, 1]!"))
}

#' @noRd
.check.graph <- function(m, v=NULL)
{
  name.graph  <- deparse(substitute(m))
  name.vector <- deparse(substitute(v))
  if (!is.matrix(m))
    stop('please provide a matrix object!')
  if (any(m < 0))
    stop(paste0("'", name.graph, "' has to be non-negative"))
  if (dim(m)[1] != dim(m)[2])
    stop(paste(name.graph, "has to be of dimension (n x n)!"))
  if (!is.null(v) && dim(m)[1] != dim(v)[1])
    stop(paste(name.vector, "has to have same dimension as", name.graph))
}

#' @noRd
.check.starting.matrix <- function(v)
{
  name <- deparse(substitute(v))
  if (any(v < 0))
    stop(paste(name, "can only contain non-negative values!"))
  if (!is.matrix(v))
    stop(paste("matrix-shaped", name, "required"))
}
