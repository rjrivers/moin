
# HAMILTONIAN MODELS ------------------------------------------------------
# TODO:
# * Add progress indicator to metropolis
# * Add input validation to metropolis
# * Define a proper output class for metropolis
# * Write pretty print() function for metropolis results
# * Write plot() function for metropolis results
# * Add more Hamiltonian models
# * Documentation
# * Unit tests
# * Profiling & optimisation


# Gravity model components ------------------------------------------------
# Conventions:
# * Coefficient are Greek letters (e.g. "alpha")
# * Capital letters are matrices (i.e. edge variables)
# * Small letters are vectors (i.e. node variables) or scalar universal constraints

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param E
##' @param Si
##' @param Sj
##' @return
##' @author
h_omega <- function(E, Si = 1, Sj = 1) {
  SiSj <- matrix(rep(Si, ncol(E)), nrow(E), ncol(E)) *
          matrix(rep(Sj, each = nrow(E)), nrow(E), ncol(E))
  sum(E * (log(E / SiSj) - 1))
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param alpha
##' @param E
##' @param f
##' @return
##' @author
h_alpha <- function(alpha, E, f) {
  alpha * (sum(E) - f)^2
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param beta
##' @param E
##' @param C
##' @param c
##' @return
##' @author
h_beta <- function(beta, E, C, c) {
  beta * (sum(E*C) - c)^2
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param gammas
##' @param E
##' @param g
##' @param margin
##' @return
##' @author
h_gamma <- function(gammas, E, g, margin) {
  sum(gammas) * (apply(E, margin, sum) - g)^2
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param delta
##' @param X
##' @param g
##' @param s
##' @return
##' @author
h_delta <- function(delta, X, g, s) {
  delta * (X - sum(g * (log(g / s) - 1)))^2
}

# Gravity models ----------------------------------------------------------

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param E
##' @param Si
##' @param Sj
##' @param f
##' @param alpha
##' @param beta
##' @param C
##' @param c
##' @return
##' @author
##' @export
h_simple_gravity <- function(E, Si = 1, Sj = 1, f, alpha, beta, C, c) {
  h_omega(E, Si, Sj) + h_alpha(alpha, E, f) + h_beta(beta, E, C, c)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param E
##' @param Si
##' @param Sj
##' @param beta
##' @param C
##' @param c
##' @param gammas
##' @param g
##' @param margin
##' @return
##' @author
##' @export
h_constrained_gravity <- function(E, Si = 1, Sj = 1, beta, C, c, gammas, g, margin = 1) {
 h_omega(E, Si, Sj) + h_beta(beta, E, C, c) + h_gamma(gammas, E, g, margin)
}

# Doubly constrained model need to check that sum of Is = sum of Os
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param E
##' @param Si
##' @param Sj
##' @param beta
##' @param C
##' @param c
##' @param in_gammas
##' @param in_g
##' @param out_gammas
##' @param out_g
##' @return
##' @author
h_double_constrained_gravity <- function(E, Si = 1, Sj = 1, beta, C, c, in_gammas, in_g, out_gammas, out_g) {
 h_omega(E, Si, Sj) + h_beta(beta, E, C, c) + h_gamma(in_gammas, E, in_g, margin = 1) + h_gamma(out_gammas, E, out_g, margin = 2)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param E
##' @param Si
##' @param Sj
##' @param beta
##' @param C
##' @param c
##' @param gammas
##' @param g
##' @param delta
##' @param X
##' @param s
##' @return
##' @author
##' @export
h_retail <- function(E, Si = 1, Sj = 1, beta, C, c, gammas, g, delta, X, s) {
 h_omega(E, Si, Sj) + h_beta(beta, E, C, c) + h_gamma(gammas, E, g, margin = 1) + h_delta(delta, X, g, s)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param E
##' @param Si
##' @param Sj
##' @param f
##' @param in_delta
##' @param out_delta
##' @param in_g
##' @param in_s
##' @param out_g
##' @param out_s
##' @return
##' @author
##' @export
h_alonso <- function(E, Si = 1, Sj = 1, f, in_delta, out_delta, in_g, in_s = rep(1, length(in_g)), out_g, out_s = rep(1, length(out_g))) {
 h_omega(E, Si, Sj) + h_alpha(alpha, E, f) + h_delta(in_delta, X, in_g, in_s) + h_delta(out_delta, X, out_g, out_s)
}

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

