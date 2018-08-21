
testdata <- sf::st_read("../vignettes/data/OldenburgerGraben/Placenames.shp") 


D <- 5000
output <- spatgraphs::spatgraph(sf::st_coordinates(testdata),
                      "geometric",
                      par=D)

output2 <- igraph::graph_from_adj_list(output$edges, mode = "all")

matrixout <- igraph::as_adjacency_matrix(output2,sparse=FALSE)

output3 <- tidygraph::as_tbl_graph(output2,directed=FALSE)




plot(output3)
