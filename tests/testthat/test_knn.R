context("knn")

test_that("knn error", {
  expect_error(knn(1,1,1))
})
