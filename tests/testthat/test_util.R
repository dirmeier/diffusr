context("util")

p0 <- c(1, rep(0, 4))
graph <-  igraph::make_graph(c(1, 2, 2, 3, 3, 4, 4, 5), directed = FALSE)
r <- .5

test_that("stochastic col norm", {
  ad <- igraph::get.adjacency(graph)
  stoch.norm <- diffusr::.stoch.col.norm(ad)
  cor.mat <- scale(ad, center=F, scale=Matrix::colSums(ad))
  expect_equal(stoch.norm, cor.mat)
})

test_that("is dgc matrix", {
  ma <- methods::as(matrix(0, 5, 5), "dgCMatrix")
  expect_true(.is.Matrix(ma))
})

test_that("is dge matrix", {
  ma <- methods::as(matrix(1, 5, 5), "dgeMatrix")
  expect_true(.is.Matrix(ma))
})

test_that("as matrix", {
  ma <- .as.matrix(graph)
  expect_true(is.matrix(ma))
})

test_that("equals double true", {
  expect_true(.equals.double(0.9, 1, .1))
})

test_that("equals double false", {
  expect_true(.equals.double(0.9, 1, .05))
})

test_that("is in true", {
  expect_true(.in(0.9, 0, 1))
})

test_that("is in false", {
  expect_true(.in(0.9, 0, .5))
})
