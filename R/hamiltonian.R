
# HAMILTONIAN MODELS ------------------------------------------------------


##outer loop
while (dH < threshold) {
    dH <- H1 - H2
    b <- exp(-beta * dH)

    ## middle loop -> "sweep"
    i_index <- sample(1:nrow(e))
    j_index <- sample(1:ncol(e))
    
    ## inner_loop
    H_old <- hamiltonian()
    hvars_old <- hvars

    hvars %>%
        map(change_vars())

    change_vars <- function() {
        for (i in 1:length(hvars)) {
        
            ## change
        
            v[i_index[i]] <- runif(1)
            e[i_index[i],j_index[i]] <- runif(1)

            H <- hamiltonian()
            b <- exp(-beta * (H_old - H))
            
            if(H < H_old ||
               runif() < b)) {
                hvars_old <- hvars_new
            }
        }
    }
    if(H < H_old) {
            ##keep
        } 
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
