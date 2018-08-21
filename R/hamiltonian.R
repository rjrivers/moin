
# HAMILTONIAN MODELS ------------------------------------------------------

hamiltonian <- function(node_data, node_formula = ~x, edges) {
  # Construct a function from ^
}

recursion <- function(mat, beta, hfunc, threshold) {
  H1 <- hfunc(mat)
  H2 <- H1 # Change one node

  dH <- H1 - H2
  b <- exp(-beta * dH)

  if (dH < threshold) {
    return(H1)
  }
  else {
    if (dH < 0 ||
        runif() < b) {
      recursion(H2)
    }
    else {
      recursion(H1)
    }
  }
}
