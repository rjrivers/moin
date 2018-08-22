#' Aggregate features by Voronoi tesselation
#' Creates a Voronoi tesselation of the nodes and aggregates features in tiles according to their spatial location. Returns a dataframe of aggregated features and their associated node as a new collum.
#' @tile aggr.fea.voro
#' @param nodes a data.frame containing metric x and y coordinates of nodes, additionally an identifier. Coordinates are expected to be the first two columns.
#' @param features a data.frame containing metric x and y coordinates of nodes, and feature type. Coordinates are expected to be the first two columns.
#'
#' @return a dataframe with feature types and their corresponding node
#' @export
#'
#' @examples
#' 

aggr.fea.voro <- function(nodes, features){
    ## Creating a global window to ensure all points will be included. Minimises/enlarges the window by +/- 1 to avoid exclusion of points at the border
    global_win <- spatstat::boundingbox(
        spatstat::union.owin(
            spatstat::owin(c(min(nodes[,1])-1,max(nodes[,1])+1),
                           c(min(nodes[,2])-1,max(nodes[,2])+1)
            ),
            spatstat::owin(c(min(features[,1])-1,max(features[,1])+1),
                           c(min(features[,2])-1,max(features[,2])+1)
            )
        ))             
    ## Apply Voronoi Tesselation to assign points to nodes
    PPP_nd <- spatstat::as.ppp(nodes[,c(1,2)], spatstat::owin(global_win))
    PPP_fea <- spatstat::as.ppp(features[,c(1,2)], spatstat::owin(global_win))
    voronoi <- spatstat::dirichlet(PPP_nd)
    PPP_assign <- spatstat::cut.ppp(PPP_fea, voronoi)
    ## Exporting results from assignment
    DF_aggr_fea <- cbind(features[3],nodes=as.numeric(PPP_assign[["marks"]]))
    
    return(DF_aggr_fea)
}