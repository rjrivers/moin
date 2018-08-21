setwd("/arch/dropboxneu/Dropbox/Dropbox/daten/git/moinP/moin/Data")
## Assigning finds to sites using buffer
library(rgeos)
library(sp)
## Loading Data
file <- "fs_ag.csv"
sites <- read.csv(file, header = TRUE, sep = ";")
file <- "shkr-weapons.csv"
artefacts <- read.table(file, sep = ";", header=TRUE)
artefacts <- artefacts[,c(1,2,13)]
## Creating SpatialPoints
SP_fs <- sp::SpatialPointsDataFrame(sites[,c(3,4)], sites)
SF_fs <- sf::st_as_sf(SP_fs) 
plot(SP_fs)
SP_art <- sp::SpatialPoints(artefacts[,c(1,2)])
SF_art <- sf::st_as_sf(SP_art)
plot(SP_art)
## Creating matrix with ALL connections
con_all <- rgeos::gDistance(SP_fs, byid=TRUE)
r_buf <- min(con_all[which(con_all>0)])/2 # radius of buffer for assigning finds to sites, using smallest nearest neighbourhood distance from distance matrix
## Deleting all geographical distances
con_all <- replace(con_all,values = NA)
## Assigning finds to sites using r_buf
buffer <- rgeos::gBuffer(SP_fs, width = r_buf)
SF_buffer <- sf::st_as_sf(buffer)
plot(SF_buffer)
out <- sf::st_intersection(SF_art,SF_buffer)
plot(out)
plot(st_coordinates(out))

o <- overlay(SF_art, SF_buffer)

blub <- st_coordinates(out)
SP_blub <- sp::SpatialPoints(blub)
plot(SP_blub)
plot(buffer, add=TRUE)
