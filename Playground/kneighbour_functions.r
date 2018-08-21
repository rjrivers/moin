
testdata <- sf::st_read("../vignettes/data/OldenburgerGraben/Placenames.shp") 

# MDM - Maximum Distance Modell
D <- 5000
mdm <- spatgraphs::spatgraph(sf::st_coordinates(testdata),
                      "geometric",
                      par=D)

spatgraph_to_tidylist <- function(x,output="igraph"){
  igraphobj <- igraph::graph_from_adj_list(x$edges, 
                                           mode = "all")
  
  nbmatrix <- igraph::as_adjacency_matrix(igraphobj,sparse=FALSE)
  
  tidygraphobj <- tidygraph::as_tbl_graph(igraphobj,directed=FALSE)
  
  return(switch(output, "igraph" = igraphobj, 
                "matrix" = nbmatrix, 
                "tidygraph" = tidygraphobj))
}

spatgraph_to_tidylist(x=mdm,output="tidygraph")

# PPA - Proximal Point Analysis

k <- 5
ppa <- spatgraphs::spatgraph(sf::st_coordinates(testdata),
                                "knn",
                                par=k)

spatgraph_to_tidylist(x=ppa,output="tidygraph")


  


