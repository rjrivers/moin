library(moin)

context("Deterrence Function")

test_that("deterrence function defaults to exponential",{
  expect_equal(deterrence_function(matrix(c(1,1,1,1),nrow=2),2),
               exp(-2 * matrix(c(1,1,1,1),nrow=2)))
  })

test_that("deterrence function gives correct results for different type",{
  expect_equal(deterrence_function(matrix(c(1,1,1,1),nrow=2),2, type = "exponential"),
               exp(-2 * matrix(c(1,1,1,1),nrow=2)))
  expect_equal(deterrence_function(matrix(c(2,2,2,2),nrow=2),2, type = "negpower"),
               matrix(c(2,2,2,2), nrow=2) ^-2)
  expect_equal(deterrence_function(matrix(c(2,2,2,2),nrow=2),2, type = "power"),
               matrix(c(2,2,2,2), nrow=2) ^2)
})

test_that("deterrence function gives an error for wrong type parameter", {
  expect_error(
  deterrence_function(matrix(c(1,1,1,1),nrow=2),2, type = "horst"),
  "Sorry, I do not know this kind of deterrence function")
})

test_that("deterrence function gives no error for correct type parameter", {
  expect_error(
    deterrence_function(matrix(c(1,1,1,1),nrow=2),2, type = "power"),
    NA)
})

#TODO test ariadne type deterrence function