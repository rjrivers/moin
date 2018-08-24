
#' moin_network
#'
#' 
#' Generates model graphs for MDM, PPA or IOM models.
#'
#' @param input a sf object containing point coordinates or a data.frame. 
#' @param method type of model graph to be produced, either a Maximum Distance Model ("mdm"), a Proximal Point Analysis ("ppa") or a full graph as base for a Intervening Opportunity Model ("iom"); defaults to "iom" i.e. a full graph.
#' 
#' @param par an integer indicating the distance threshold value D for method "mdm" and the neighbour rank k for "ppa" .
#' @param coords_x a character string; defines the column name of the X-coordinate in case input is a data.frame; Defaults to NA. 
#' @param coords_y a character string; defines the column name of the Y-coordinate in case input is a data.frame; Defaults to NA. 
#' @param crs coordinate reference system: integer with the EPSG code, or character with proj4string.
#'
#' @details To find EPSG codes see e.g.  \href{http://epsg.io/}{EPSG.io} or package \href{https://cran.r-project.org/web/packages/rgdal/index.html}{rgdal}.
#' @return a list with
#' \itemize{
#' \item{"graph"}{ = a graph object of class tidygraph }
#' \item{"distance_matrix"}{ = a matrix of all interpoint distances (given in meter)}
#' \item{"neighbourhood_matrix"}{ = an adjacency (or neighbourhood) matrix, where 0 = not connected nodes and 1 = connected nodes.}
#' \item{"truncated_distance_matrix"}{ = a matrix, where cell(ij) = distance(ij) if nodes are connected else 0.}
#' \item{"input_data"}{ = the sf object supplied as input.}
#' }
#' @export
#' 
#' @author Hendrik Raese <h.raese@ufg.uni-kiel.de>
#' @author Georg Roth <georg.roth@fu-berlin.de>
#' @author Wolfgang Hamer <hamer@geographie.uni-kiel.de>
#' 
#' @examples
#' set.seed(1985)
#' d <- data.frame(matrix(runif(15), ncol = 3))
#' p <- sf::st_as_sf(x = d, coords = 1:2)
#' result <- moin_network(input = p, method = "mdm", par = 0.3)
#' plot(result$graph,sf::st_coordinates(result$input_data))

moin_network <- function(input, method="iom", par=NULL, mode="undirected", coords_x=NA,coords_y=NA, crs=NA,...){

  if(any(class(input)=="sf")){
    input <- input
  }else if(class(input)=="data.frame"){
    input <- sf::st_as_sf(x = input, coords = c(coords_x,coords_y),crs=crs)
  }else{
    stop("Unknown data input. Please use object of class sf or data.frame!")
  }
  
  
  dista <- sf::st_distance(x = input, 
                           y = input)
  
  dista2 <- matrix(dista,nrow=nrow(dista))
  
  nb_mat <- switch(method, 
                   "iom"=1*(dista2>0),
                   "mdm"=(1*(dista2>0))*(1*(dista2 <= par)),
                   "ppa"=apply(dista2, 2,FUN = function(x){( order(order(x))<=(par+1)& order(order(x))>1)*1}))
  
  graph <- igraph::graph_from_adjacency_matrix(nb_mat, mode=mode)
  
  tidygraphobj <- tidygraph::as_tbl_graph(graph)
  
  truncated <- nb_mat * dista2
  
  return(list(graph=tidygraphobj, 
              distance_matrix=dista2, 
              neighbourhood_matrix= nb_mat, 
              truncated_distance_matrix=truncated, 
              input_data=input))
}
