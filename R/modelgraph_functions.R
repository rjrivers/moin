#' modelgraph
#' 
#' Basically a wrapper to produce knn or geometric graphs with spatgraphs.
#'
#' @param sfobj a sf object containing locations of points 
#' @param type defining if a Maximum Distance Modell ("mdm") or a Proximal Point Analysis ("ppa") is used to generate the network
#' @param par for type "mdm" the threshold value D, and for "ppa" neighbour rank k
#'
#' @return a graph of class "sg" (see package \href{https://cran.r-project.org/web/packages/spatgraphs/index.html}{spatgraphs} for specifics)
#' @export
#'
#' @examples
#' set.seed(1985)
#' d <- data.frame(matrix(runif(15), ncol = 3))
#' p <- st_as_sf(x = d, coords = 1:2)
#' result <- modelgraph(sfobj = p, type = "mdm", par = 0.3)
#' plot(result,st_coordinates(p))

modelgraph <- function(sfobj,type,par){
  return(
    spatgraphs::spatgraph(sf::st_coordinates(sfobj),
                          ifelse(type=="mdm","geometric","knn"),
                          par=par)
  )
}