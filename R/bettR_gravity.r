rihll_wilson <- function(Oi, Wj, cij, alpha = 1, beta = 1, detfun = "power") {
  check_input(Oi, Wj, cij, alpha, beta, detfun)
  
  prepare_data
  
  do_calculation
  
  return(formated(result))
}

do_calculation <- function(Oi, Wj, fcij){
  
  Tij <- calculate_tij(Oi, Wj, fcij)
  Dj <- calculate_dj(Tij)
  calculate_new_wij
}

calculate_tij <- function(Oi, Wj, fcij) {
  for( i in 1:length(Oi)) {
    for( j in 1:length(Wj)) {
      Tij[i,j] <- Oi[i] * ((Wj[j] * fcij[i,j]) / sum(Wj * fcij[i,], na.rm = TRUE))
    }
  }
  return(Tij)
}

calculate_dj <- function(Tij) {
  # Dj <- rep(NA, time = ncol(Tij))
  # for (i in 1:ncol(Tij)) {
  #   Dj[i] <- sum(Tij[, i])
  # }
  Dj <- colSums(Tij)
  return(Dj)
}

Tij <- matrix(1:9, ncol=3)

calculate_dj(Tij)
