# DETERRENCE FUNCTIONS ---------------------------------------------------------

minimum_distance <- function(mat, min) {
  mat[mat < min] <- NA

  # Warn if some nodes are "stranded" from the rest of the graph
  if(rowSums(is.na(mat)) == ncol(mat) ||
     colSums(is.na(mat)) == nrow(mat) ) {
    warning("Some nodes are not connected to the graph. Check min value.")
  }

  return(mat)
}

#' @title Inverse power function
#' @description calculate the cost of distances based on an inverse power law
#' @param mat a distance matrix
#' @param power the power to be used; default 2
#' @examples
#' distmat <- matrix(runif(n=64),8,8)
#' diag(distmat) <- 0
#' inverse_power(mat = distmat, power= 2)
#' @export
inverse_power <- function(mat, power = 2) {
    mat <- (1/mat)^power
    mat[mat==Inf] <- 0
    return(mat)
}
