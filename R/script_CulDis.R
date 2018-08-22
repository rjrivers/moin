setwd("/arch/dropboxneu/Dropbox/Dropbox/daten/git/moinP/moin/Data")
## Loading Data
file <- "fs_ag.csv"
nodes <- read.csv(file, header = TRUE, sep = ";")
file <- "shkr-weapons.csv"
features <- read.table(file, sep = ";", header=TRUE)
features <- features[,c(1,2,4)]

## Aggregating features by voronoi
library(spatstat)
result <- aggr.fea.voro(nodes,features)

## Calculating Cultural Distances, returing Cultural Distance Matrix

dummy <- data.frame(node=c(1:18), a= c(18:1), b=c(7,15,3,28,9,12,1,15,15,31,1,8,7,9,99,47,42,86),
                    c=c(3,15,3,28,9,12,1,25,15,31,1,8,5,9,99,85,65,86))

cul_dis_dummy <- dist.matr(dummy)


