calculate_tij <- function(Oi, Wj, fcij, alpha) {
  
  Wj <- Wj^alpha
  fcij <- as.matrix(fcij)
  
  Tij <- apply((fcij * Oi %o% Wj), 2, `/`, t(Wj %*% t(fcij)))
  
  rownames(Tij) <- rownames(fcij)

  return(Tij)
}

rihll_wilson <- function(Oi, Wj, fcij, alpha, eps = 1e-6, maxrun = 1000){

  iter <- 0
  
  Dj <- Wj
  
  epsilon <- 1
  
  while(!(epsilon<eps)&!(iter>maxrun)){
    
    Wj <- Dj
    
    Tij <- calculate_tij(Oi,Wj,fcij, alpha)
    
    Dj <- colSums(Tij, na.rm = TRUE)
    
    iter <- iter+1
    epsilon <- sum((Dj - Wj)^2)
  }
  
  names(Dj) <- rownames(fcij)
  rval <- list(Inputs = Dj,
               Tij = Tij,
               nb.iter = iter)
  return(rval)
}
