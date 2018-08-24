
# HAMILTONIAN MODELS -----------------------------------------------------------
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


# Maximum entropy (gravity) models ---------------------------------------------
# Conventions:
# * Coefficient are Greek letters (e.g. "alpha")
# * Capital letters are matrices (i.e. edge variables)
# * Small letters are vectors (i.e. node variables) or scalar universal
# *   constraints

#' Entropy part of the gravity model
#'
#' @title h_omega
#' @param E a matrix of random values (to be "optimized" by the Hamiltonian)
#' @param Si a vectory of node attributes, e.g. carrying capacity or population
#' @param Sj a vectory of node attributes, e.g. carrying capacity or population
#' @return a scalar
#' @author Daniel Knitter <\email{knitter@@geographie.uni-kiel.de}>
#' @author Joe Roe <\email{jwg983@@hum.ku.dk}>
#' @author Ray Rivers <\email{r.rivers@@imperial.ac.uk}>
h_omega <- function(E, Si = 1, Sj = 1) {
  SiSj <- matrix(rep(Si, ncol(E)), nrow(E), ncol(E)) *
          matrix(rep(Sj, each = nrow(E)), nrow(E), ncol(E))
  sum(E * (log(E / SiSj) - 1))
}

#' The general flow constrained of the gravity model
#'
#' @title h_alpha
#' @param alpha lagrange multiplier
#' @param E a matrix of random values (to be "optimized" by the Hamiltonian)
#' @param f a scalar giving the general systems constrained
#' @return a scalar
#' @author Daniel Knitter <\email{knitter@@geographie.uni-kiel.de}>
#' @author Joe Roe <\email{jwg983@@hum.ku.dk}>
#' @author Ray Rivers <\email{r.rivers@@imperial.ac.uk}>
h_alpha <- function(alpha, E, f) {
  alpha * (sum(E) - f)^2
}

#' The cost constrained of the gravity model
#'
#' @title h_beta
#' @param beta langrange multiplier
#' @param E a matrix of random values (to be "optimized" by the Hamiltonian)
#' @param C distance matrix
#' @param c distance constraining cost of the system
#' @return a scalar
#' @author Daniel Knitter <\email{knitter@@geographie.uni-kiel.de}>
#' @author Joe Roe <\email{jwg983@@hum.ku.dk}>
#' @author Ray Rivers <\email{r.rivers@@imperial.ac.uk}>
h_beta <- function(beta, E, C, c) {
  beta * (sum(E*C) - c)^2
}

#' The in- or outflow constrained of the gravity model
#'
#' @title h_gamma
#' @param gammas lagrange multiplier vector
#' @param E a matrix of random values (to be "optimized" by the Hamiltonian)
#' @param g constraining vector
#' @param margin constraining dimension (either sum of rows or columns)
#' @return a scalar
#' @author Daniel Knitter <\email{knitter@@geographie.uni-kiel.de}>
#' @author Joe Roe <\email{jwg983@@hum.ku.dk}>
#' @author Ray Rivers <\email{r.rivers@@imperial.ac.uk}>
h_gamma <- function(gammas, E, g, margin) {
  if (length(g)==1) {
    g <- rep(g, dim(E)[margin])
  }

  sum(gammas * (apply(E, margin, sum) - g)^2)
}

#' The node characteristics constrained of the gravity model
#'
#' @title h_delta
#' @param delta lagrange multiplier
#' @param x general node constraining value
#' @param g a vector representing node characteristics 
#' @param s a vector of node characteristics
#' @return a scalar
#' @author Daniel Knitter <\email{knitter@@geographie.uni-kiel.de}>
#' @author Joe Roe <\email{jwg983@@hum.ku.dk}>
#' @author Ray Rivers <\email{r.rivers@@imperial.ac.uk}>
h_delta <- function(delta, x, g, s) {
  delta * (x - sum(g * (log(g / s) - 1)))^2
}

#' Simple gravity model
#'
#' @title h_simple_gravity
#' @param E a matrix of random values (to be "optimized" by the Hamiltonian)
#' @param Si a vectory of node attributes, e.g. carrying capacity or population
#' @param Sj a vectory of node attributes, e.g. carrying capacity or population
#' @param f a scalar giving the general systems constrained
#' @param alpha lagrange multiplier
#' @param beta lagrange multiplier
#' @param C distance matrix
#' @param c distance constraining cost of the system
#' @return a scalar
#' @author Daniel Knitter <\email{knitter@@geographie.uni-kiel.de}>
#' @author Joe Roe <\email{jwg983@@hum.ku.dk}>
#' @author Ray Rivers <\email{r.rivers@@imperial.ac.uk}>
#' @export
h_simple_gravity <- function(E, Si = 1, Sj = 1, f, alpha, beta, C, c) {
  h_omega(E, Si, Sj) + h_alpha(alpha, E, f) + h_beta(beta, E, C, c)
}

#' Constrained gravity model
#'
#' @title h_constrained gravity
#' @param E a matrix of random values (to be "optimized" by the Hamiltonian)
#' @param Si a vectory of node attributes, e.g. carrying capacity or population
#' @param Sj a vectory of node attributes, e.g. carrying capacity or population
#' @param beta lagrange multiplier
#' @param C distance matrix
#' @param c distance constraining cost of the system
#' @param gammas lagrange multiplier vector
#' @param g constraining vector
#' @param margin constraining dimension (either sum of rows or columns)
#' @return a scalar
#' @author Daniel Knitter <\email{knitter@@geographie.uni-kiel.de}>
#' @author Joe Roe <\email{jwg983@@hum.ku.dk}>
#' @author Ray Rivers <\email{r.rivers@@imperial.ac.uk}>
#' @export
h_constrained_gravity <- function(E, Si = 1, Sj = 1, beta, C, c, gammas, g,
                                  margin = 1) {
 h_omega(E, Si, Sj) + h_beta(beta, E, C, c) + h_gamma(gammas, E, g, margin)
}

#' Double constrained gravity model
#'
#' @title h_double_constrained_gravity
#' @param E a matrix of random values (to be "optimized" by the Hamiltonian)
#' @param Si a vectory of node attributes, e.g. carrying capacity or population
#' @param Sj a vectory of node attributes, e.g. carrying capacity or population
#' @param beta lagrange multiplier
#' @param C distance matrix
#' @param c distance constraining cost of the system
#' @param in_gammas lagrange multiplier vector
#' @param in_g vector of inflows (rows)
#' @param out_gammas lagrange multiplier vector
#' @param out_g vector of outflows (columns)
#' @return a scalar
#' @author Daniel Knitter <\email{knitter@@geographie.uni-kiel.de}>
#' @author Joe Roe <\email{jwg983@@hum.ku.dk}>
#' @author Ray Rivers <\email{r.rivers@@imperial.ac.uk}>
#' @export
h_double_constrained_gravity <- function(E, Si = 1, Sj = 1, beta, C, c,
                                         in_gammas, in_g, out_gammas, out_g) {
  if(sum(in_g)!=sum(out_g)) {
    warning("Your input does not match the model assumptions. The inflows have to be equal to the outflows.")
  }

 h_omega(E, Si, Sj) +
    h_beta(beta, E, C, c) +
    h_gamma(in_gammas, E, in_g, margin = 1) +
    h_gamma(out_gammas, E, out_g, margin = 2)
}

#' Retail Gravity model
#'
#' @title h_retail
#' @param E a matrix of random values (to be "optimized" by the Hamiltonian)
#' @param Si a vectory of node attributes, e.g. carrying capacity or population
#' @param Sj a vectory of node attributes, e.g. carrying capacity or population
#' @param beta lagrange multiplier
#' @param C distance matrix
#' @param c distance constraining cost of the system
#' @param gammas lagrange multiplier vector
#' @param g constraining vector
#' @param delta lagrange multiplier
#' @param x general node constraining value
#' @param s a vector of node characteristics
#' @return a scalar
#' @author Daniel Knitter <\email{knitter@@geographie.uni-kiel.de}>
#' @author Joe Roe <\email{jwg983@@hum.ku.dk}>
#' @author Ray Rivers <\email{r.rivers@@imperial.ac.uk}>
##' @export
h_retail <- function(E, Si = 1, Sj = 1, beta, C, c, gammas, g, delta, x, s) {
 h_omega(E, Si, Sj) +
    h_beta(beta, E, C, c) +
    h_gamma(gammas, E, g, margin = 1) +
    h_delta(delta, x, g, s)
}

#' Alonso gravity model
#'
#' @title h_alonso
#' @param E a matrix of random values (to be "optimized" by the Hamiltonian)
#' @param Si a vectory of node attributes, e.g. carrying capacity or population
#' @param Sj a vectory of node attributes, e.g. carrying capacity or population
#' @param f a scalar giving the general systems constrained
#' @param in_delta general node constraining value of inflows
#' @param out_delta general node constraining value of outflows
#' @param in_g vector of inflows (rows)
#' @param in_s vector of inflows (rows)
#' @param out_g vector of outflows (rows)
#' @param out_s vector of outflows (rows)
#' @return a scalar
#' @author Daniel Knitter <\email{knitter@@geographie.uni-kiel.de}>
#' @author Joe Roe <\email{jwg983@@hum.ku.dk}>
#' @author Ray Rivers <\email{r.rivers@@imperial.ac.uk}>
#' @export
h_alonso <- function(E, Si = 1, Sj = 1, f, in_delta, out_delta,
                     in_g, in_s = rep(1, length(in_g)),
                     out_g, out_s = rep(1, length(out_g)), alpha, x) {
 h_omega(E, Si, Sj) +
    h_alpha(alpha, E, f) +
    h_delta(in_delta, x, in_g, in_s) +
    h_delta(out_delta, x, out_g, out_s)
}

#' MCMC/Metropolis algorithm for solving Hamiltonian functions
#'
#' Description
#'
#' @param hfunc a Hamiltonian function either of the Ariadne or one of the gravity models
#' @param hvars a list of named arguments to the selected Hamiltonian function
#' @param hconsts a list of named constants to the selected Hamiltonian function
#' @param beta a scalar; the cooling factor of the "hot Hamiltonian landscape"
#' @param threshold a scalar; stopping threshold
#' @param threshold_window a scalar; the size of the moving average window calculating the stopping threshold
#' @param silent a boolean indicating whether you want to see information of changes in beta and the Hamiltonian per iteration
#'
#' @return a list
#'
#' @author Daniel Knitter <\email{knitter@@geographie.uni-kiel.de}>
#' @author Joe Roe <\email{jwg983@@hum.ku.dk}>
#' @author Ray Rivers <\email{r.rivers@@imperial.ac.uk}>
#'
#' @export
metropolis <- function(
  hfunc, hvars, hconsts, hvar_constraints,
  beta = 100, beta_prod = 2,
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

#' Hamiltonian ariadne models
#'
#' An implementation of Evans, Rivers & Knappett's ``ariadne`` model; a
#' Hamiltonian, cost-benefit network model.
#'
#' @param s a vector of node values (e.g. carrying capacity)
#' @param v a vector of node values (e.g. exploitation of the carry capacity)
#' @param D a matrix of edge deterrence values
#' @param E a matrix of edge weight values (e.g. intensity of interaction)
#' @param k scalar lagrange multiplier for the kappa term
#' @param l scalar lagrange multiplier for the lambda term
#' @param j scalar lagrange multiplier for the jay term
#' @param u scalar lagrange multiplier for the mu term
#'
#' @return the scalar Hamiltonian value
#'
#' @examples
#'
#' @author Daniel Knitter <\email{knitter@@geographie.uni-kiel.de}>
#' @author Joe Roe <\email{jwg983@@hum.ku.dk}>
#' @author Ray Rivers <\email{r.rivers@@imperial.ac.uk}>
#'
#' @export
h_ariadne <- function(s, v, D, E, k, l, j, u) {
  kappa <- sum(s * v * (1 - v))
  lambda <- sum(matrix(s*v, 1, length(s)) %*% (D * E) %*% matrix(s*v, length(s), 1))
  jay <- sum(s * v)
  mu <- sum(matrix(s*v, 1, length(s)) %*% E)

  H <- -(k*kappa) - (l*lambda) + (j*jay) + (u*mu)
  return(H)
}
