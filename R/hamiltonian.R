
# HAMILTONIAN MODELS ------------------------------------------------------


hvars <- NULL # Initial state
beta <- NULL  # Starting temperature
odH <- 0
while (odH > threshold) {
  oH1 <- hfunc(hvars)

  # TODO: Maybe repeat for n sweeps as Evans does?
  for (h in 1:length(hvars)) {
    ishuffle <- sample(1:length(hvars[h]))
    for (i in 1:length(hvars[h])) {
      hvars2 <- hvars
      hvars2[[h]][ishuffle[[i]]] <- runif()

      H1 <- hfunc(hvars)
      H2 <- hfunc(hvars2)
      dH <- H1 - H2
      b <- exp(-beta * dH)

      if(dH < 0 ||
         runif() < b) {
          hvars <- hvars2
          rm(hvars2)
      }
    }
  }

  oH2 <- hfunc(hvars)
  odH <- oH1 - oH2

  beta <- beta * 2
}




hamiltonian <- function(node_data, node_formula = ~x, edges) {
  # Construct a function from ^
}

# set diagonals to 0?
# S vector
# v vector (random 0-1)
# d deterrence matrix
# e matrix (random 0-1)
# k scalar constant
# l scalar constant
# j scalar constant
# u scalar constant
h_ariadne <- function(S, v, d, e, k, l, j, u) {
  kappa <- sum(S * v * (1 - v))
  lambda <- sum(matrix(S*v, 1, length(S)) %*% (d * e) %*% matrix(S*v, length(S), 1))
  jay <- sum(S * v)
  mu <- sum(matrix(S*v, 1, length(S)) %*% e)

  H <- -(k*kappa) - (l*lambda) + (j*jay) + (u*mu)
  return(H)
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
