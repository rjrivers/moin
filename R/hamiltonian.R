
# HAMILTONIAN MODELS ------------------------------------------------------


# Gravity model components ------------------------------------------------
# Conventions:
# * Coefficient are Greek letters (e.g. "alpha")
# * Capital letters are matrices (i.e. edge variables)
# * Small letters are vectors (i.e. node variables) or universal scalar

h_omega <- function(E, Si = 1, Sj = 1) {
  SiSj <- matrix(rep(Si, ncol(E)), nrow(E), ncol(E)) *
          matrix(rep(Sj, each = nrow(E)), nrow(E), ncol(E))
  sum(E * (log(E / SiSj) - 1))
}

h_alpha <- function(alpha, E, f) {
  alpha * (sum(E) - f)^2
}

h_beta <- function(beta, E, C, c) {
  beta * (sum(E*C) - c)^2
}

h_gamma <- function(gammas, E, g, margin) {
  sum(gammas) * (apply(E, margin, sum) - g)^2
}

h_delta <- function(delta, X, g, s) {
  delta * (X - sum(g * (log(g / s) - 1)))^2
}


# Gravity models ----------------------------------------------------------

h_simple_gravity <- function(E, F) {
  h_omega() + h_alpha() + h_beta()
}

h_constrained_gravity <- function() {

}

h_double_constrained_gravity <- function() {

}

h_retail <- function() {

}

h_ariadne <- function() {

}

# Doubly constrained model need to check that sum of Is = sum of Os

# TODO:
# * Validate ARIADNE model
# * Allow specifying distribution for updating hvars
# * Add useful messages + statistics + graphs?
# * Handle large beta values (convert to BigInt?)
# * add a progress indicator to communicate that the functions is still running...
# * More Hamiltonian models
# * Profiling & optimisation

#' MCMC/Metropolis algorithm for solving Hamiltonian functions
#'
#' Description
#'
#' @author Daniel Knitter <\email{knitter@@geographie.uni-kiel.de}>
#' @author Joe Roe <\email{jwg983@@hum.ku.dk}>
#' @author Ray Rivers <\email{r.rivers@@imperial.ac.uk}>
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

# hcomponent_0
# hcomponent_alpha
# hcomponent_beta
# hcomponent_rho
#
# hcomponent_delta
# hcomponent_epsilon
