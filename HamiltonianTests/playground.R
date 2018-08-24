## Example Applications

## Data setup
data(medieval_settlements, package = "moin")

distmat <- medieval_settlements %>%
 `[`(1:5,) %>%  
  sf::st_as_sf(coords = c("x","y"),
               crs = 25832) %>%  
  as("Spatial") %>%
  rgeos::gDistance(byid = TRUE) %>%
  `/`(1000)

costs <- distmat %>%
  moin::inverse_exponentional()

test_d <- matrix(runif(length(costs)),ncol(costs),nrow(costs))

## H-Ariadne
res <- hamiltonian_metrop(
  hfunc = h_ariadne,
  hvars = list(v = runif(ncol(costs)),
               E = test_d),
  hconsts = list(s = rep(1,ncol(costs)),
                 D = costs,
                 k = 1,
                 l = 1,
                 j = 1,
                 u = 1),
  hvar_constraints = list(v = c(0,1),
                          E = c(0,1)),
  beta = 100,
  threshold = 1e-01)

library(ggplot2)
res$model_iterations %>%
  ggplot(aes(x = i)) +
  geom_line(aes(y=meanH),color="green") +
  geom_line(aes(y=H)) +
  scale_y_log10()

## simple gravity
res <- hamiltonian_metrop(hfunc = h_simple_gravity,
                          hvars = list(E = test_d),
                          hconsts = list(beta = 64,
                                         alpha = 64,
                                         C = costs,
                                         c = 1,
                                         f = 1),
                          hvar_constraints = list(E = c(0,1)),
                          beta = 100,
                          threshold = 1)
res

## Alonso
res <- hamiltonian_metrop(hfunc = h_alonso,
                          hvars = list(E = test_d),
                          hconsts = list(f = 1,
                                         in_g = rep(.1,ncol(costs)),
                                         out_g = rep(.1,ncol(costs)),
                                         alpha = 64,
                                         x = 2,
                                         in_delta = 2,
                                         out_delta = 3),
                          hvar_constraints = list(E = c(0,1)),
                          beta = 100,
                          threshold = 1)

res <- hamiltonian_metrop(hfunc = h_constrained_gravity,
                          hvars = list(E = test_d),
                          hconsts = list(c = 4,
                                         gammas = rep(.1,ncol(costs)),
                                         g = .1,
                                         C = costs,
                                         beta = 64),
                          hvar_constraints = list(E = c(0,1)),
                          beta = 100,
                          threshold = 1)

res <- hamiltonian_metrop(hfunc = h_double_constrained_gravity,
                          hvars = list(E = test_d),
                          hconsts = list(c = 4,
                                         in_gammas = rep(.1,ncol(costs)),
                                         in_g = .1,
                                         out_gammas = rep(.1,ncol(costs)),
                                         out_g = .1,
                                         C = distmat,
                                         beta = 64),
                          hvar_constraints = list(E = c(0,1)),
                          beta = 100,
                          threshold = 50)

res <- hamiltonian_metrop(hfunc = h_retail,
                          hvars = list(E = test_d),
                          hconsts = list(c = 4,
                                         s = rep(.1,ncol(costs)),
                                         gammas = rep(.1,ncol(costs)),
                                         g = .1,
                                         C = costs,
                                         delta = 2,
                                         x = 3,
                                         beta = 64),
                          hvar_constraints = list(E = c(0,1)),
                          beta = 100,
                          threshold = 1)
