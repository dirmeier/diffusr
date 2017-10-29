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

context("heat diffusion")

h <- c(1, rep(0, 4))
adja <- matrix(1, 5, 5)
diag(adja) <- 0

test_that("heat diffusion with zero diffusion",{
  s <- heat.diffusion(h, adja, 0)
  expect_equal(as.vector(s), h, 0.001)
})

test_that("heat diffusion if maximum diffusion",{
  expect <- rep(0.2,  5)
  s <- heat.diffusion(h, adja, 10000000)
  expect_equal(as.vector(s), expect, 0.001)
})

test_that("heat diffusion matrix if maximum diffusion",  {
  p0 <- matrix(0, nrow=5, ncol=10)
  p0[1,] <- 1
  expect <- matrix(1/5, nrow=5, ncol=10)
  mat.heat <- heat.diffusion(p0, adja, 1000000)
  expect_equal(mat.heat, expect, 0.0001)
})

test_that("heat diffusion vectorial is same as with matrix",  {
  p0 <- matrix(rep(1:10, each=5), nrow=5)
  mat.heat <- heat.diffusion(p0, adja, 1000000)
  vec.heat <- sapply(seq(ncol(p0)), function(e) {
      p <- heat.diffusion(p0[, e], adja, .5)
      p
  })
  expect_equal(mat.heat, vec.heat, 0.0001)
})

test_that("wrong t negative values", {
  expect_error(heat.diffusion(rep(-1, 5), adja, -1))
})

test_that("wrong t class", {
  expect_error(heat.diffusion(rep(-1, 5), adja, "s"))
})
