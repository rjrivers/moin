library(devtools)
library(moin)
library(testthat)


context("test Rihll and Wilson function")

#function(Oi, Wj, fcij, alpha
test_that("function gives an error for wrong parameter class", {
  expect_error(
    calculate_tij( (matrix(c(1,2,1,2),nrow=2)), rep(1, time=5), (matrix(c(1,2,1,2),nrow=2)), 1.2))
  expect_error(
    calculate_tij(rep(1, time=5), (matrix(c(1,3,1,2),nrow=2)), (matrix(c(1,2,1,2),nrow=2)), 1.2))
  expect_error(
    calculate_tij(rep(1, time=5),rep(1, time=5), "A", 1.2))
  expect_error(
    calculate_tij(rep(1, time=2),rep(1, time=2), (matrix(c(1,2,1,2),nrow=2)), (matrix(c(1,2,1,2),nrow=2)))
      )})


test_that("Oi and Wj have same length", {
  expect_equal(
    calculate_tij(rep(1, time=2),
                  rep(1, time=2),
                  matrix(c(1,2,1,2),nrow=2), 1.2) , 
    apply(matrix(c(1,2,1,2),nrow=2) * rep(1, time=2) %o% (rep(1, time=2))^1.2,
          2,
          `/`,
          t((rep(1, time=2))^1.2) %*% t(matrix(c(1,2,1,2),nrow=2))
  ))
})

  