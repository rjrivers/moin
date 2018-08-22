library(moin)


# testdata <- sf::st_read("../vignettes/data/OldenburgerGraben/Placenames.shp") 

testdata <- sf::st_read("../vignettes/data/settlements.shp") 

# generating centroids of the polygons of the example data 
testdata <- sf::st_centroid(testdata)


modelgraph2 <- function(testdata, method="iom", par=NULL, mode="undirected")
  {
dista <- sf::st_distance(x = testdata, 
                         y = testdata)

dista2 <- matrix(dista,nrow=nrow(dista))

nb.mat <- switch(method, 
       "iom"=1*(dista2>0),
       "mdm"=(1*(dista2>0))*(1*(dista2 <= par)),
       "ppa"=apply(dista2, 2,FUN = function(x){( order(order(x))<=(par+1)& order(order(x))>1)*1}))

graph <- igraph::graph_from_adjacency_matrix(nb.mat, mode=mode)

tidygraphobj <- tidygraph::as_tbl_graph(graph)

truncated <- nb.mat * dista2

return(list(graph=tidygraphobj, distance.matrix=dista2, neighbourhood.matrix= nb.mat, truncated.distance.matrix=truncated  ))

}

res <- modelgraph2( testdata)
plot(res$graph,
     vertex.size=3,
     vertex.label.cex=0.25,
     layout=sf::st_coordinates(testdata))

View(res$neighbourhood.matrix)
