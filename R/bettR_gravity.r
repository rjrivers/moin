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
    #Ai_new <- 1 / colSums(Ij_new^alpha * fcij)
    Ai_new <- (colSums(Ij_new^alpha * fcij))^-1
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


cij <- read.csv2("tests/testthat/LTD1costs_10sites.csv", row.names = 1)

beta <- .01
fcij <- exp(-beta * cij)

alpha <- 1.1
Oi <- rep(1,time = nrow(cij))

rw_result <- list()
rw_result$Inputs <- rihll_wilson(Oi, fcij, alpha)[[2]]
rw_result$Inputs

rw_result_2 <- rwgm(as.matrix(cij), alpha, beta)

display.input(rw_result, sorted = T)
display.input(rw_result_2, sorted = T)



display.input <- function(rwgm.result,sorted=FALSE) {
  opar <- par(mar=c(2.5,9,0.5,0.5))
  inputs <- rwgm.result$Inputs
  if(sorted) {
    inputs <- sort(inputs)
  }
  barplot(inputs,horiz=TRUE,las=1,cex.names=0.65)
  par(opar)
}

rwgm <- function(costs,alpha,beta,nmax=10000,eps=1e-6,step=0.01,random=FALSE) {
  expC <- exp(-beta*costs)
  Outputs <- rep(1,nrow(costs))
  if(random) {
    Inputs <- runif(nrow(costs))
  } else {
    Inputs <- rep(1,nrow(costs))
  }
  for(l in 1:nmax) {
    A <- expC%*%(Inputs^alpha)
    T <- sweep(outer(Outputs,(Inputs^alpha))*expC,1,A,"/")
    n.Inputs <- Inputs+step*(colSums(T)-Inputs)
    ##        n.Inputs <- colSums(T)
    ##        print(paste(l,sum((n.Inputs-Inputs)^2)))
    if(sum((n.Inputs-Inputs)^2)<eps) break
    Inputs <- n.Inputs
  }
  list(Inputs=Inputs,T=T,nb.iter=l)
}

testdata <- matrix(c(0,150,120,120,150,60,120,30,30,30), nrow=2)

cij <- as.matrix(dist(t(testdata)))

