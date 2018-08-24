###############################
###### GRAVITY FUNCTIONS ######



#' Calculate Tij
#' 
#' Calculate Tij
#' 
#' @param Oi Vector of the ouputs of i
#' @param Wj Vector of the inputs of j
#' @param fcij matrix giving the deterrence function
#' @param alpha scaling factor for attractivity
#' 
#' @author Martin Hinz <martin.hinz@iaw.unibe.ch>
calculate_tij <- function(Oi, Wj, fcij, alpha) {
  
  Wj <- Wj^alpha
  fcij <- as.matrix(fcij)
  
  Tij <- apply((fcij * Oi %o% Wj), 2, `/`, t(Wj %*% t(fcij)))
  
  rownames(Tij) <- rownames(fcij)
  
  return(Tij)
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

####################################################################################

#' Simple Gravity Model
#' 
#' Simple Gravity Model
#' Mass = ability of entities to spread flows = ex : Mass or Population
#' 
#' @param Mi vector of mass for every site
#' @param fcij deterrence function from the distance matrix (has to be produced from a distance matrix before)
#' @param k adjustment variable
#' 
#' @return A matrix with the modeled flows from i to j
#' 
#' @author Clara Filet <clara.filet@gmail.com>
#' 
#' @export simple_gravity
simple_gravity <- function(Mi, fcij, k=1) {
  Mmatrix <- Mi%o%Mi
  Tij <- k*((Mmatrix)/fcij) 
  return(Tij)
}

##################################
###### DETERRENCE FUNCTIONS ######

#' Calculates the deterrence function
#' 
#' Calculates the deterrence function used for the different gravity based models
#' 
#' @param cij a matrix containing the cost matrix
#' @param beta the distance decay factor
#' @param type the family of deterrence function to use
#' @param alpha shape parameter 1 for ariadne type deterrence function
#' @param gamma shape parameter 2 for ariadne type deterrence function
#' 
#' @details Power is generally used for simple gravity models, exponential and ariadne can be used for the Rihll-Wilson model.
#' \code{alpha} and \code{gamma} are only relevant for the ariadne type model and default to the values alpha=4 and gamma=1 like in Evans&Rivers 2017.
#' 
#' 
#' @references Evans Tim S., Rivers Ray J., Was Thebes Necessary? Contingency in Spatial Modeling. Frontiers in Digital Humanities 4, 2017, \url{https://doi.org/10.3389/fdigh.2017.00008}
#' 
#' @return a matrix containing the deterrence function
#' 
#' @author Daniel Knitter <knitter@geographie.uni-kiel.de>
#' @author Martin Hinz <martin.hinz@iaw.unibe.ch>
#' @author Benedikt Grammer	<benedikt.grammer@univie.ac.at>
#' @author Kai Radloff <kai.radloff@hu-berlin.de>
#' @author Loren V. Cowin	<Lcowin@gshdl.uni-kiel.de>
#' @author Clara Filet <clara.filet@gmail.com>
#' 
#' @export deterrence_function
deterrence_function <- function(cij,beta,type = 'exponential', alpha=4, gamma=1) {
  if (type=='exponential'){
    fcij <- exp(-beta * cij)  
  }
  else if (type == 'negpower'){
    fcij <- cij^-beta 
  }
  else if (type == 'power'){
    fcij <- cij^beta 
  }
  else if (type == 'ariadne'){
    fcij <- (1 + (cij^beta)^alpha)^-gamma
  }
  else {
    stop ("Sorry, I do not know this kind of deterrence function")
  }
  return(fcij)
}
