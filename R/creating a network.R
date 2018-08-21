setwd("/arch/dropboxneu/Dropbox/Dropbox/daten/git/moinP/moin/Data")
library(sp)
library(deldir)
library(spdep)
library(tidygraph)
library(igraph)
library(rgeos)
## Loading Data
file <- "fs_ag.csv"
sites <- read.csv(file, header = TRUE, sep = ";")
## Creating SpatialPoints
coords <- sites[,c(3,4)]
fs <- SpatialPointsDataFrame(coords, sites)
plot(fs)
## creating a delaunay 
gDel <- gDelaunayTriangulation(fs, tolerance=0.0, onlyEdges=FALSE)
plot(gDel)
## creating tidygraph object
blub <- graph_from_edgelist(fs_nb_del, directed = TRUE)


