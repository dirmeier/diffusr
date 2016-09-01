context("knn")

test_that("knn neighbors path",{
  n <- 10
  adj <- rbind(cbind(0, diag(n-1)), 0)
  for (i in 1:(n-1))
  {
    nei <- unlist(neighbors(as.integer(1), adj, i, F))
    expect_equal(nei, 2:(i + 1))
  }
})

test_that("knn neighbors tree",{
  n <- 10
  adj <- matrix(0, n, n)
  adj[1,2] <- adj[2,3] <-  adj[3,4] <- adj[3,5] <- 1
  nei <- unlist(neighbors(as.integer(1), adj, 3, F))
  expect_equal(sort(nei), c(2,3,4,5))
})

test_that("knn neighbors non-positiv idx",{
  n <- 10
  adj <- rbind(cbind(0, diag(n-1)), 0)
  expect_error(neighbors(as.integer(0), adj, 2, F))
})

test_that("knn neighbors non-matrix elem",{
  expect_error(neighbors(as.integer(0), "s", 2, F))
})

test_that("knn neighbors non-numeric k",{
  n <- 10
  adj <- rbind(cbind(0, diag(n-1)), 0)
  expect_error(neighbors(as.integer(0), adj, "s", F))
})

test_that("knn neighbors non-logical use weights",{
  n <- 10
  adj <- rbind(cbind(0, diag(n-1)), 0)
  expect_error(neighbors(as.integer(0), adj, 1, "F"))
})

test_that("knn neighbors true edge weights",{
  n <- 10
  adj <- rbind(cbind(0, diag(n-1)), 0)
  expect_error(neighbors(as.integer(0), adj, 2, T))
})

test_that("knn .neighbors neg elems",{
  n <- 10
  adj <- rbind(cbind(0, diag(n-1)), 0)
  adj[1,1] <- -1
  expect_error(.neighbors(1, adj, i, F))
})

test_that("knn .neighbors wrong dim",{
  n <- 10
  adj <- cbind(0, diag(n-1))
  expect_error(.neighbors(1, adj, i, F))
})

test_that("knn .neighbors path",{
  n <- 10
  adj <- rbind(cbind(0, diag(n-1)), 0)
  for (i in 1:(n-1))
  {
    nei <- unlist(.neighbors(1, adj, i, F))
    expect_equal(nei, 2:(i + 1))
  }
})
