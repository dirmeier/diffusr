context("mrw")

p0 <- c(1, rep(0, 4))
graph <-  igraph::make_graph(c(1, 2, 2, 3, 3, 4, 4, 5), directed = FALSE)
r <- .5

test_that("random walk if w restarts", {
  s <- random.walk(p0, graph, 1)
  expect_equal(s, p0)
})

test_that("random walk if w/o restarts", {
  s <- random.walk(p0, Matrix::as.matrix(igraph::get.adjacency(graph)), 0)
  expect_equal(s, p0)
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

ptt <- p0
pold <- rep(1, 5)
r <- 0
while (sum(abs(ptt-pold)) > .0001)
{
  pold <- ptt
  ptt <- (1-r)* s %*% ptt +  r * p0
}

