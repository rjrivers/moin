library(sf)
raw <- read_sf("Placenames.shp")
sites <- raw %>%
    `[`(1:10,)
distmat <- st_distance(sites)
