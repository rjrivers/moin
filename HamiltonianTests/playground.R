
raw <- sf::read_sf("Placenames.shp")
sites <- raw %>%
    `[`(1:10,)

distmat <- sf::st_distance(sites)

distmat <- sites %>%
    as("Spatial") %>%
    rgeos::gDistance(byid = TRUE) %>%
    `/`(1000)

costs <- distmat %>%
    inverse_power()

costs <- distmat %>%
    inverse_exponentional()

test_d <- matrix(runif(625),25,25)

res <- hamiltonian_metrop(hfunc = h_ariadne,
                   hvars = list(v = runif(10),
                                e = matrix(runif(100),10,10)),
                   hconsts = list(S = rep(1,10),
                                  d = costs,
                                  k = 1,
                                  l = 1,
                                  j = 1,
                                  u = 1),
                   beta = 100,
                   threshold = 1e-01)
res

# TODO: "e" in the function should be created automatically based on the size of d

test_d <- matrix(runif(25),5,5)

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
