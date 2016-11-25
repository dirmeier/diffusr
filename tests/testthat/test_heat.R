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

context("heat")

h <- c(1, rep(0, 4))
adja <- matrix(1, 5, 5)

test_that("heat diffusion with zero diffusion",{
  s <- heat.diffusion(h, adja, 0)
  expect_equal(s, h)
})

test_that("heat diffusion if maximum diffusion",{
  expect <- rep(0.2,  5)
  s <- heat.diffusion(h, adja, 1)
  expect_equal(s, expect)
})

