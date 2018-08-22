
# HAMILTONIAN MODELS ------------------------------------------------------

#' Title
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
hamiltonian_metrop <- function(hfunc, hvars, hconsts, beta = 100, thresholds = c(0.001, 0.01)) {
  updates <- rep(1, length(thresholds))
  while (!all(updates < thresholds)) {
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
        b <- exp(beta * dH)

        #message("h: ", h, "; i: ", i, "; dH: ", dH)

        if(dH > 0 ||
           runif(1) < b) {
            hvars <- hvars2
            rm(hvars2)
        }
      }
    }

    updates <- mapply(function(old, new) {
      sum(old != new) / length(old)
    }, old_hvars, hvars, USE.NAMES = FALSE)
    print(updates)
    beta <- beta * 2
    print(mean(hvars$v))
    message("beta: ", beta)
  }

  return(hvars)
}



hamiltonian <- function(node_data, node_formula = ~x, edges) {
  # Construct a function from ^
}

# set diagonals to 0?
# S vector
# v vector (random 0-1)
# d deterrence matrix
# e matrix (random 0-1)
# k scalar constant
# l scalar constant
# j scalar constant
# u scalar constant
#' Title
#'
#' @param S
#' @param v
#' @param d
#' @param e
#' @param k
#' @param l
#' @param j
#' @param u
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

#' @export
test_k <- function(v) {
    sum(v * (1-v))
}

