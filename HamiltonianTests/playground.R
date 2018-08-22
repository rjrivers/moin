
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

hamiltonian_metrop(hfunc = h_ariadne,
                   hvars = list(v = runif(25),
                                e = matrix(runif(625),25,25)),
                   hconsts = list(S = rep(1,25),
                                  d = test_d,
                                  k = 1,
                                  l = 0,
                                  j = 0,
                                  u = 0),
                   beta = 100,
                   threshold = 1e-02)
