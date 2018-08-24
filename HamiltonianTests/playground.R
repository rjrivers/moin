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
               e = test_d),
  hconsts = list(S = rep(1,ncol(costs)),
                 d = costs,
                 k = 1,
                 l = 1,
                 j = 1,
                 u = 1),
  hvar_constraints = list(v = c(0,1),
                          e = c(0,1)),
  beta = 100,
  threshold = 1e-01)

## h_simple_gravity
res <- hamiltonian_metrop(hfunc = h_simple_gravity,
                          hvars = list(E = test_d),
                          hconsts = list(c = 4,
                                         f = 2,
                                         C = costs,
                                         alpha = 64,
                                         beta = 64),
                          hvar_constraints = list(E = c(0,1)),
                          beta = 100,
                          threshold = 1)


