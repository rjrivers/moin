#' Creates a network based on the moin_network function and combines the adjacency matrix with the cultural distance matrix.
#' 
#' @title moin_cult_network
#' 
#' @param nodes_x a vector containing metric x coordinates of nodes
#' @param nodes_y a vector containing metric y coordinates of nodes
#' @param nodes_id a vector containing ID for nodes
#' @param features a data.frame containing metric x and y coordinates of features, and feature type. Coordinates are expected to be the first two columns.
#' @param type_col a character string naming the columname containing feature types.
#' @param pre_size numeric, amount of letters, e.g. characters before typenumbers. Defaults to 1.
#' @param method character string, the distance measure to be 
#'   used ("euclidean", "maximum", "manhattan", "canberra", 
#'   "binary" or "minkowski"). Defaults to euclidean distance.
#' @param net_method type of model graph to be produced, either a Maximum Distance Model ("mdm"), a Proximal Point Analysis ("ppa") or a full graph as base for a Intervening Opportunity Model ("iom"); defaults to "iom" i.e. a full graph.
#' @param par an integer indicating the distance threshold value D for method "mdm" and the neighbour rank k for "ppa" .
#' @param mode a character string; default is undirected. Directed networks are not supported!
#' @param coords_x a character string; defines the column name of the X-coordinate in case input is a data.frame; Defaults to NA. 
#' @param coords_y a character string; defines the column name of the Y-coordinate in case input is a data.frame; Defaults to NA. 
#' @param crs coordinate reference system: integer with the EPSG code, or character with proj4string.
#' @param plotted a Boolean operator defining whether a plot should be created. Defaults to FALSE. Edge widths are scaled by maximum distance values and enlarged by factor 2. 
#' 
#' @return a list containing a graph object of classes tidygraph ("tbl_graph") resp. igraph ("igraph") and the cultural distance matrix. 
#'
#' @author Wolfgang Hamer <\email{hamer@@geographie.uni-kiel.de}>
#' @author Chiara Girotto <\email{chiara.girotto@@web.de}>
#' @author Hendrik Raese <\email{h.raese@@ufg.uni-kiel.de}>
#' @author Carolin Tietze <\email{ctietze1991@@yahoo.com}>
#' @author Michael Bilger <\email{sofnod@@googlemail.com}>
#' 
#' @examples
#' # Example mdm network
#'set.seed(1234)
#'
#'moin_cult_network(nodes_x=sample(3433806:3581396, 10, replace = TRUE),
#'                  nodes_y=sample(5286004:5484972, 10, replace = TRUE), 
#'                  nodes_id=c(1:10), 
#'                  features=data.frame(x=sample(3433806:3581396, 100, replace = TRUE),
#'                                      y=sample(5286004:5484972, 100, replace = TRUE),
#'                                      type=paste0("B", c(rep(1, 5), rep(2,15), sample(11:19, 20, replace = TRUE), 
#'                                                         sample(111:119, 30, replace = TRUE), sample(1111:1115, 30, replace = TRUE)))
#'                  ), 
#'                  type_col="type" , pre_size=1, method = "euclidean", plotted = TRUE,network_method ="mdm", par = 50000, mode = "undirected", crs = NA)
#'
#' # Example ppa network 
#'set.seed(1234)
#'moin_cult_network(nodes_x=sample(3433806:3581396, 10, replace = TRUE),
#'nodes_y=sample(5286004:5484972, 10, replace = TRUE), 
#'nodes_id=c(1:10), 
#'features=data.frame(x=sample(3433806:3581396, 100, replace = TRUE),
#'                    y=sample(5286004:5484972, 100, replace = TRUE),
#'                    type=paste0("B", c(rep(1, 5), rep(2,15), sample(11:19, 20, replace = TRUE), 
#'                                       sample(111:119, 30, replace = TRUE), sample(1111:1115, 30, replace = TRUE)))
#'), 
#'type_col="type" , pre_size=1, method = "euclidean", plotted = TRUE,network_method ="ppa", par = 3, mode = "undirected", crs = NA)

#' @export 


moin_cult_network <- function(nodes_x,nodes_y, nodes_id, features, type_col, pre_size=1, method = "euclidean", network_method ="iom", par = NULL, mode = "undirected", crs = NA, plotted = FALSE){
  
  cult_dis <- moin_cult_dist(nodes_x = nodes_x,
                             nodes_y = nodes_y, 
                             nodes_id = nodes_id,
                             features = features, 
                             type_col = type_col,
                             pre_size = pre_size, 
                             method = method, 
                             plotted = FALSE)
  
  base_net <- moin_network(input = data.frame(nodes_x,nodes_y,nodes_id), 
                           method = network_method, 
                           par = par, 
                           mode = mode, 
                           coords_x = "nodes_x",
                           coords_y = "nodes_y", 
                           crs=crs)
  
  trunc_matrix <- base_net$neighbourhood_matrix*cult_dis$cult_dist_matr
  
  trunc_matrix0 <- trunc_matrix
  
  trunc_matrix0[is.na(trunc_matrix0)]<-0
  
  trunc_net <- igraph::graph_from_adjacency_matrix(trunc_matrix0, mode = mode, weighted = TRUE, diag = FALSE)
  igraph::V(trunc_net)$label = colnames(trunc_matrix0)
  
  if(plotted){
    plot(trunc_net,layout=as.matrix(data.frame(x=nodes_x,y=nodes_y)), 
         edge.width = (igraph::E(trunc_net)$weight/max(igraph::E(trunc_net)$weight))*2,
         main = "Cultural Distance truncated network")
  }
  
  tidygraphobj <- tidygraph::as_tbl_graph(trunc_net)
  return(list(tidygraph = tidygraphobj, trunc_cult_dist_matr = trunc_matrix))
  

}