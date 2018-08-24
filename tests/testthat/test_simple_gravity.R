library (moin)

context ("Simple Gravity model")

Mi <- c(2,2)
k <- 2
fcij <- matrix(rep(1/3,4), nrow=2)

test_that ("Calculating Tij from friction constant k, matrix of mass and
deterence function on cost matrix", {
  expect_equal ({
    Mmatrix <- Mi%o%Mi
    k*((Mmatrix)/fcij)},
    simple_gravity(Mi, fcij, k)
  )
                                                   
})

test_that ("Output is matrix", {
  
  Tij <- simple_gravity(Mi, fcij, k)
  
  expect_is(Tij, "matrix")
  
})

# Test against 'ground truth' from Thomas&Hugget 1980, 138
test_that ("tij matrix from a simple gravity model", {
  expect_equal({
    round(simple_gravity(c(100, 50, 150),
                   deterrence_function(
                     matrix(c(7.5,30,50,30,7.5,40,50,40,10), nrow=3), 2, type="power")
    ), 1)},
    matrix(c(177.8, 5.6, 6, 5.6, 44.4, 4.7, 6, 4.7, 225), nrow=3)
  )
})