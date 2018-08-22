## Creating matrix with ALL connections
## Deleting all geographical distances
con_all <- rgeos::gDistance(SP_fs, byid=TRUE)
con_all <- replace(con_all,values = NA)