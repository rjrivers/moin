
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
