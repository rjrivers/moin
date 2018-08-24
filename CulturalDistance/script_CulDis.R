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

typelist <- create_type_generator(features, type_col="type", pre_size=1)


## Calculating Cultural Distances, returing Cultural Distance Matrix

 result_cul_dis<- cul_dist(nodes_x=nodes[,1], nodes_y=nodes[,2],nodes_id = nodes[,3],features,type_col=3, pre_size=1)


test <- as.matrix(
      dist(as.matrix(
        type_spectra[,2:ncol(type_spectra)]),
        diag=TRUE, upper=TRUE) )

rownames(test) <- colnames(test) <- type_spectra[,1]


dist_matr <- function(df){
  l <- length(df[,1])
  con_all <- matrix(data = NA, nrow = l, ncol = l)
  for(i in 1:(l-1)){
    a <- df[i,-1]
    for(j in ((i+1):l)){
      b <- df[j,-1]
      dis <- edist(a,b)
      con_all[i,j] <- dis
      con_all[j,i] <- dis
    }
    con_all[i,i] <- 0
    con_all[l,l] <- 0
  }
  return(con_all)
}

 
blub <- dist_matr(type_spectra, method="euclidean")
