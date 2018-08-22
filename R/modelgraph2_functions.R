
#' modelgraph2
#'
#' 
#' Generates model graphs for MDM, PPA or IOM models.
#'
#' @param sfobj a sf object containing point coordinates 
#' @param method type of model graph to be produced, either a Maximum Distance Modell ("mdm"), a Proximal Point Analysis ("ppa") or a full graph as base for a Intervening Opportunity Model ("iom"); defaults to "iom" i.e. a full graph.
#' 
#' @param par for type "mdm" the threshold value D, and for "ppa" neighbour rank k
#'
#' @return a list with
#' \itemize{
#' \item{"graph"}{ = a graph object of class tidygraph }
#' \item{"distance.matrix"}{ = a matrix of all interpoint distances (given in meter)}
#' \item{"neighbourhood.matrix"}{ = an adjacency (or neighbourhood) matrix, where 0 = not connected nodes and 1 = connected nodes.}
#' \item{"truncated.distance.matrix"}{ = a matrix, where cell(ij) = distance(ij) if nodes are connected else 0.}
#' \item{"input.data"}{ = the sf object supplied as input.}
#' }
#' @export
#' @examples
#' set.seed(1985)
#' d <- data.frame(matrix(runif(15), ncol = 3))
#' p <- sf::st_as_sf(x = d, coords = 1:2)
#' result <- modelgraph2(sfobj = p, method = "mdm", par = 0.3)
#' plot(result$graph,sf::st_coordinates(result$input.data))

modelgraph2 <- function(sfobj, method="iom", par=NULL, mode="undirected")
{
  dista <- sf::st_distance(x = sfobj, 
                           y = sfobj)
  
  dista2 <- matrix(dista,nrow=nrow(dista))
  
  nb.mat <- switch(method, 
                   "iom"=1*(dista2>0),
                   "mdm"=(1*(dista2>0))*(1*(dista2 <= par)),
                   "ppa"=apply(dista2, 2,FUN = function(x){( order(order(x))<=(par+1)& order(order(x))>1)*1}))
  
  graph <- igraph::graph_from_adjacency_matrix(nb.mat, mode=mode)
  
  tidygraphobj <- tidygraph::as_tbl_graph(graph)
  
  truncated <- nb.mat * dista2
  
  return(list(graph=tidygraphobj, distance.matrix=dista2, neighbourhood.matrix= nb.mat, truncated.distance.matrix=truncated, input.data=sfobj  ))
}
