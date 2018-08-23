setwd("/arch/dropboxneu/Dropbox/Dropbox/daten/git/moinP/moin/Data")
## Assigning finds to node using buffer
library(spatstat)
## Loading Data
file <- "fs_ag.csv"
nodes <- read.csv(file, header = TRUE, sep = ";")
file <- "shkr-weapons.csv"
features <- read.table(file, sep = ";", header=TRUE)
features <- features[,c(1,2,3)]

## Using Voronoi
result <- aggr_fea_voro(nodes,features, type_col = 3)







## Assigning finds to node using r_buf
library(rgeos)

SF_nd <- sf::st_as_sf(SP_nd)
SF_fea <- sf::st_as_sf(SP_fea)

con_all <- rgeos::gDistance(SP_nd, byid=TRUE)
r_buf <- min(con_all[which(con_all>0)])/2 # radius of buffer for assigning finds to node, using smallest nearest neighbourhood distance from distance matrix
buffer <- rgeos::gBuffer(SP_nd, width = r_buf)
SF_buffer <- sf::st_as_sf(buffer)
plot(SF_buffer)
out <- sf::st_intersection(SF_fea,SF_buffer)
plot(out)
plot(st_coordinates(out))

o <- overlay(SF_fea, SF_buffer)

blub <- st_coordinates(out)
SP_blub <- sp::SpatialPoints(blub)
plot(SP_blub)
plot(buffer, add=TRUE)


