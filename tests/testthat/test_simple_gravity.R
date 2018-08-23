library (moin)

context ("Simple Gravity model")

Mi <- c(2,2)
k <- 2
fcij <- matrix(rep(1/3,4), nrow=2)

test_that ("Calculating Tij from friction constant k, matrix of mass and
deterence function on cost matrix", {
  expect_equal ({
    Mmatrix <- Mi%o%Mi
    diag(Mmatrix)<- 0
    k*((Mmatrix)/fcij)},
    simple_gravity(Mi, fcij, k)
  )
                                                   
})

test_that ("Output is matrix", {
  
  Tij <- simple_gravity(Mi, fcij, k)
  
  expect_is(Tij, "matrix")
  
})