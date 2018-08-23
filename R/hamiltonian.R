
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
hamiltonian_metrop <- function(hfunc, hvars, hconsts, hvar_constraints, beta = 100, beta_prod = 2,
                               threshold = .001, threshold_window = 50,
                               silent = FALSE) {
  
  original_beta <- beta
  # Metropolis loop
  Hs <- vector()
  meanH_save <- vector()
  repeat {
    # Iterate over all variables in the model
    old_hvars <- hvars
    for (h in 1:length(hvars)) {
      ishuffle <- sample(1:length(hvars[[h]]))
      for (i in 1:length(hvars[[h]])) {
        # Pick a new value for the variable from a uniform random distribution
        hvars2 <- hvars
        hvars2[[h]][ishuffle[i]] <- runif(n = 1,
                                          min = hvar_constraints[[h]][1],
                                          max = hvar_constraints[[h]][2])

        # Calculate the Hamiltonian for old and new states
        H1 <- do.call(hfunc, c(hvars, hconsts))
        H2 <- do.call(hfunc, c(hvars2, hconsts))
        dH <- H1 - H2

        # Accept new value if H is lowered OR based on a stochastically based on
        # the Boltzmann distribution
        if (dH == 0) b <- 0
        else b <- exp(beta * dH)

        if(dH > 0 || runif(1) < b) {
          hvars <- hvars2
          rm(hvars2)
        }
      }
    }

    # Check for equilibrium
    H_last <- do.call(hfunc, c(hvars, hconsts))
    Hs <- c(Hs, H_last)
    if ( length(Hs) > threshold_window ) {
      d_meanH <- abs(mean(Hs[length(Hs)-threshold_window:length(Hs)]) - H_last)
    }
    else {
      d_meanH <- abs(mean(Hs) - H_last)
    }

    meanH_save <- c(meanH_save, mean(Hs))
    
    if (!silent) message("∆ mean H: ", d_meanH)
    if (length(Hs) > threshold_window &&
        d_meanH < threshold) {
      break
    }

    # Decrease "temperature"
    beta <- beta * beta_prod
    if (!silent) message("Beta: ", beta)
  }

  return(list(model_parameters = c(beta = original_beta,
                                  beta_prod = beta_prod,
                                  threshold = threshold,
                                  threshold_window = threshold_window),
              hvars = hvars,
              hconsts = hconsts,
              model_iterations = data.frame(i = 1:length(meanH_save),
                                            H = Hs,
                                            meanH = meanH_save)))
  
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


## TODO: plot_hamiltonian_results function
## library(ggplot2)
## res %>%
##   ggplot(aes(x = i)) +
##   geom_line(aes(y=d)) +
##   geom_line(aes(y=H)) +
##   scale_y_log10()


#' Title
#'
#' Description
#'
#' @param alpha scalar (random)
#' @param beta scalar (random)
#' @param E matrix (random 0-1)
#' @param c cost matrix
#' @param C scalar constant
#' @param F scalar constant
#' 
#' @return
#' @export
#'
#' @examples
h_gravity <- function(alpha,
                      beta,
                      E,
                      c,
                      C,
                      F) {
  H <- sum(E * (log(E) - 1 )) + (alpha * (sum(E) - F)^2) + (beta * (sum(E*c)-C)^2)  
  return(H)
}


#' Title
#'
#' Description
#'

#' @param alpha scalar (random)
#' @param beta scalar (random)
#' @param E matrix (random 0-1)
#' @param c cost matrix
#' @param C scalar constant
#' @param F scalar constant
#' 
#' @return
#' @export
#'
#' @examples
h_singly_constrained_gravity_model <- function(alpha,
                                        beta,
                                        E,
                                        c,
                                        C,
                                        F) {
  H <- sum(E * (log(E) - 1 )) +
    (alpha * (sum(E) - F)^2) +
    (beta * (sum(E*c)-C)^2) +
    (gamma * sum(matrix(O, 1, length(O)) %*% E)^2)
  return(H)
}

hcomponent_0
hcomponent_alpha
hcomponent_beta 
hcomponent_rho

hcomponent_delta 
hcomponent_epsilon
