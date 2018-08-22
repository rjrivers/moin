rihll_wilson_old <- function(Oi, Wj, cij, alpha = 1, beta = 1, detfun = "power") {

  # todo
  check_input(Oi, Wj, cij, alpha, beta, detfun)
  
  # TODO
  prepare_data
  
  # TODO
  do_calculation
  
  # TODO
  return(formated(result))
}

#' Calculates the Rihll-Wilson-Model
#' 
#' Calculates the Rihll-Wilson-Model
#' 
#' @param Oi Outflows originating from i
#' @param fcij Deterrence function (has to be calculated from the distance
#'        cost matrix "cij" first)
#' @param alpha The scaling factor of attractivity
#' 
#' @return a list with the elements:
#' \itemize{
#' \item a vector containing Ai
#' \item a vector containing Ij 
#' }
#' 
#' @export rihll_wilson
#' 
rihll_wilson <- function(Oi, fcij, alpha) {
#   Ai and Ij represent initial values to start the while-loop
    Ij <- rep(2, times = ncol(fcij))
    Ai <- rep(2, times = nrow(fcij))
#   Ai_new and Ij_new are set to initial values (initial values must not be 0
#   because otherwise a unique solution is not guaranteed)
    Ij_new <- rep(1, times = ncol(fcij))
    Ai_new <- rep(1, times = nrow(fcij))
#   Until equilibrium is reached
  while(Ij != Ij_new || Ai != Ai_new) {
#   update old values
    Ij <- Ij_new
    Ai <- Ai_new
#   calculate new value (Evans / Rivers 2017)
    Ij_new <- rowSums(Ai * Oi * Ij^alpha * fcij)
    Ai_new <- 1 / colSums(Ij_new^alpha * fcij)
  }
  return(list(Ai_new,Ij_new))
}

  
do_calculation <- function(Oi, Wj, fcij){
  
  result <- rihll_wilson(Oi, fcij, alpha)  
  Ai  <- result[1]
  Ij  <- result[2]

  # has to be adapted  
  Tij <- calculate_tij(Oi, Wj, fcij)
}

# currently static case taken from Daniels script
calculate_tij <- function(Oi, Wj, fcij) {
  for( i in 1:length(Oi)) {
    for( j in 1:length(Wj)) {
      Tij[i,j] <- Oi[i] * ((Wj[j] * fcij[i,j]) / sum(Wj * fcij[i,], na.rm = TRUE))
    }
  }
  return(Tij)
}

# 'leftover' from static implementation
calculate_dj <- function(Tij) {
  # Dj <- rep(NA, time = ncol(Tij))
  # for (i in 1:ncol(Tij)) {
  #   Dj[i] <- sum(Tij[, i])
  # }
  Dj <- colSums(Tij)
  return(Dj)
}

# Test area, has to be clean in the final version

Tij <- matrix(1:9, ncol=3)

calculate_dj(Tij)
