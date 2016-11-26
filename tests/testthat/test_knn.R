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

context("knn")

test_that("knn neighbors path",{
  n <- 10
  adj <- rbind(cbind(0, diag(n-1)), 0)
  for (i in 1:(n-1))
  {
    nei <- unlist(nearest.neighbors(as.integer(1), adj, i, F))
    expect_equal(nei, 2:(i + 1))
  }
})

test_that("knn neighbors tree",{
  n <- 10
  adj <- matrix(0, n, n)
  adj[1,2] <- adj[2,3] <-  adj[3,4] <- adj[3,5] <- 1
  nei <- unlist(nearest.neighbors(as.integer(1), adj, 3, F))
  expect_equal(sort(nei), c(2,3,4,5))
})

test_that("knn neighbors non-positiv idx",{
  n <- 10
  adj <- rbind(cbind(0, diag(n-1)), 0)
  expect_error(nearest.neighbors(as.integer(0), adj, 2, F))
})

test_that("knn neighbors non-matrix elem",{
  expect_error(nearest.neighbors(as.integer(0), "s", 2, F))
})

test_that("knn neighbors non-numeric k",{
  n <- 10
  adj <- rbind(cbind(0, diag(n-1)), 0)
  expect_error(nearest.neighbors(as.integer(0), adj, "s", F))
})

test_that("knn neighbors non-logical use weights",{
  n <- 10
  adj <- rbind(cbind(0, diag(n-1)), 0)
  expect_error(nearest.neighbors(as.integer(0), adj, 1, "F"))
})

test_that("knn neighbors true edge weights",{
  n <- 10
  adj <- rbind(cbind(0, diag(n-1)), 0)
  expect_error(nearest.neighbors(as.integer(0), adj, 2, T))
})

