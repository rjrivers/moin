# DETERRENCE FUNCTIONS ---------------------------------------------------------

#' Maximum distance model of deterrence
#'
#' Creates a deterrence matrix from a distance matrix based on a simple maximum
#' interaction distance threshold.
#'
#' @param mat a distance matrix
#' @param max maximum distance of interaction
#'
#' @return a matrix of integer deterrences (0 or 1)
#' @export
#'
#' @examples
#' distmat <- matrix(runif(n=64),8,8)
#' diag(distmat) <- 0
#' mdm(distmat, 0.1)
mdm <- function(mat, max) {
  mat %>%
    is_less_than(min) %>%
    multiply_by(1) -> # Cast to int
    mat

  # Warn if some nodes are "stranded" from the rest of the graph
  if(rowSums(mat) == 0 ||
     colSums(mat) == 0) {
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

#' @title Inverse exponential function
#' @description calculate the cost of distances based on an inverse exponential function
#' @param mat a distance matrix
#' @param beta ..
#' @examples
#' distmat <- matrix(runif(n=64),8,8)
#' diag(distmat) <- 0
#' inverse_exponential(mat = distmat, beta = 1)
#' @export
inverse_exponentional <- function(mat, beta = .1 {
    mat <- exp(-(beta * mat))
    mat[mat==Inf] <- 0
    return(mat)
}

