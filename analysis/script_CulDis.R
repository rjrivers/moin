setwd("/arch/dropboxneu/Dropbox/Dropbox/daten/git/moinP/moin/Data")
## Loading Data
file <- "fs_ag.csv"
nodes <- read.csv(file, header = TRUE, sep = ";")
file <- "shkr-weapons.csv"
features <- read.table(file, sep = ";", header=TRUE)
features <- features[,c(1,2,3)]

## Aggregating features by voronoi
library(spatstat)
result <- aggr.fea.voro(nodes,features)

## Calculating Cultural Distances, returing Cultural Distance Matrix

 result_cul_dis<- cul_dist(nodes_x=nodes[,1], nodes_y=nodes[,2],nodes_id = nodes[,3],features,type_col=3, pre_size=1)

