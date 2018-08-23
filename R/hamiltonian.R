
# HAMILTONIAN MODELS ------------------------------------------------------

# TODO:
# * Validate ARIADNE model
# * Allow specifying distribution for updating hvars
# * Add useful messages + statistics + graphs?
# * Handle large beta values (convert to BigInt?)
# * add a progress indicator to communicate that the functions is still running...
# * More Hamiltonian models
# * Profiling & optimisation

#' Title
#'
#' Description
#'
#' @param hfunc
#' @param hvars
#' @param hconsts
#' @param beta
#' @param thresholds
#'
#' @return
#' @export
#'
#' @examples
hamiltonian_metrop <- function(hfunc, hvars, hconsts, beta = 100,
                               threshold = .001, min_iterations = 50,
                               silent = FALSE) {
  dh_out <- vector()
  repeat {
    old_hvars <- hvars

    # TODO: Maybe repeat for n sweeps as Evans does?
    for (h in 1:length(hvars)) {
      ishuffle <- sample(1:length(hvars[[h]]))
      for (i in 1:length(hvars[[h]])) {
        hvars2 <- hvars
        hvars2[[h]][ishuffle[i]] <- runif(1)

        #print(hvars2)

        H1 <- do.call(hfunc, c(hvars, hconsts))
        H2 <- do.call(hfunc, c(hvars2, hconsts))
        dH <- H1 - H2

        if (dH == 0) {
          b <- 0
        }
        else {
          b <- exp(beta * dH)
        }

        if(dH > 0 || runif(1) < b) {
          hvars <- hvars2
          rm(hvars2)
        }
      }
    }

    dh_last <- do.call(hfunc, c(hvars, hconsts))
    dh_out <- c(dh_out, dh_last)
    dh_diff <- abs(mean(dh_out)-dh_last)
    beta <- beta * 2

    if (!silent) {
      message("Mean ∆H: ", dh_diff)
      message("Beta: ", beta)
    }

    if (length(dh_out) > min_iterations
        && dh_diff < threshold) {
      break
    }
  }
  return(hvars)
}

#' Title
#'
#' Description
#'
#' @param S vector
#' @param v vector (random 0-1)
#' @param d deterrence matrix
#' @param e matrix (random 0-1)
#' @param k scalar constant
#' @param l scalar constant
#' @param j scalar constant
#' @param u scalar constant
#'
#' @return
#' @export
#'
#' @examples
h_ariadne <- function(S, v, d, e, k, l, j, u) {
  kappa <- sum(S * v * (1 - v))
  lambda <- sum(matrix(S*v, 1, length(S)) %*% (d * e) %*% matrix(S*v, length(S), 1))
  jay <- sum(S * v)
  mu <- sum(matrix(S*v, 1, length(S)) %*% e)

  H <- -(k*kappa) - (l*lambda) + (j*jay) + (u*mu)
  return(H)
}
