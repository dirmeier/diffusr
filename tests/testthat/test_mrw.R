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

context("mrw")

p0 <- c(1, rep(0, 4))
adja <- matrix(1, 5, 5)
graph <-  igraph::graph_from_adjacency_matrix(adja)

test_that("random walk if w restarts", {
  s <- random.walk(p0, graph, 1)
  expect_equal(s, p0)
})

test_that("random walk if w/o restarts", {
  s <- random.walk(p0, Matrix::as.matrix(igraph::get.adjacency(graph)), 0)
   expect_equal(s, rep(.2, 5))
})

test_that("random walk with mat for graph", {
  s <- random.walk(p0, igraph::get.adjacency(graph), 1)
  expect_equal(s, p0)
})

test_that("random walk with dge for mat", {
  s <- random.walk(p0, Matrix::as.matrix(igraph::get.adjacency(graph)), 1)
  expect_equal(s, p0)
})

test_that("random walk if false p0", {
  expect_error(random.walk(p0 + .1, graph, 1))
})

test_that("random walk if false mat", {
  expect_error(random.walk(p0, 2, 1))
})

test_that("random walk if p0 not numeric", {
  expect_error(random.walk(1L, graph, 1))
})

test_that("random walk if r not in", {
  expect_error(random.walk(p0, graph, 2))
})

test_that("random walk if r not numeric", {
  expect_error(random.walk(p0, graph, "s"))
})

test_that("random walk if smaller zero", {
  expect_error(.rwr(p0, matrix(rnorm(24), 4), 1))
})

test_that("random walk if false dim", {
  expect_error(.rwr(p0, matrix(abs(rnorm(24)), 4), 1))
})

test_that("random walk p0 smaller zero", {
  expect_error(random.walk(p0 - 5, graph, 1))
})
