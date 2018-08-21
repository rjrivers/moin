
raw <- sf::read_sf("Placenames.shp")
sites <- raw %>%
    `[`(1:10,)

distmat <- sf::st_distance(sites)

distmat <- sites %>%
    as("Spatial") %>%
    rgeos::gDistance(byid = TRUE)
