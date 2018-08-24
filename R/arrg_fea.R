#' Aggregate features by Voronoi tesselation
#' 
#' Creates a Voronoi tesselation of the nodes and aggregates features in tiles according to their spatial location. Returns a dataframe of aggregated features and their associated node as a new collum.
#' 
#' @title aggr_fea_voro
#' 
#' @param nodes a data.frame containing metric x and y coordinates of nodes, additionally an identifier. Coordinates are expected to be the first two columns.
#' @param features a data.frame containing metric x and y coordinates of nodes, and feature type. Coordinates are expected to be the first two columns.
#' @param type_col a character string naming the columname containing feature types.
#'
#' @return a dataframe with feature types and their corresponding node. Additionally a
#'     plot is created to display the aggregation of features to nodes. Amount of feature
#'     per node is added as number to the plot.
#' 
#' @author Oliver Nakoinz <\email{oliver.nakoinz@@ufg.uni-kiel.de}>
#' @author Chiara Girotto <\email{chiara.girotto@@web.de}>
#' @author Franziska Faupel <\email{franziska-faupel@@gmx.de}>
#'
#' @examples
#' 
#' @export

aggr_fea_voro <- function(nodes, features, type_col){
    ## Creating a global window to ensure all points will be included. Minimises/enlarges the window by +/- 1 to avoid exclusion of points at the border
    global_win <- global_bb(nodes, features)         
    ## Apply Voronoi Tesselation to assign points to nodes
    ppp_nd <- spatstat::as.ppp(nodes[,c(1,2)], spatstat::owin(global_win))
    ppp_fea <- spatstat::as.ppp(features[,c(1,2)], spatstat::owin(global_win))
    voronoi <- spatstat::dirichlet(ppp_nd)
    ppp_assign <- spatstat::cut.ppp(ppp_fea, voronoi)
        ## Exporting results from assignment
    df_aggr_fea <- cbind(features[type_col],nodes=as.numeric(ppp_assign[["marks"]]))
    
    plot_voro_fea(df_aggr_fea, nodes, ppp_nd, voronoi)
    
    return(df_aggr_fea)
}

#' Create global bounding box
#' 
#' @title global_bb
#' @param nodes a data.frame containing metric x and y coordinates of nodes, additionally an identifier. Coordinates are expected to be the first two columns.
#' @param features a data.frame containing metric x and y coordinates of nodes, and feature type. Coordinates are expected to be the first two columns.
#'
#' @return an owin object which is slightly bigger, to avoid missing points due to edge effects
#' @author Oliver Nakoinz <\email{oliver.nakoinz@@ufg.uni-kiel.de}>
#' @author Chiara Girotto <\email{chiara.girotto@@web.de}>
#' @author Franziska Faupel <\email{franziska-faupel@@gmx.de}>

global_bb <- function(nodes, features){
  result_bb <-  spatstat::boundingbox(
  spatstat::union.owin(
    spatstat::owin(c(min(nodes[,1])-1,max(nodes[,1])+1),
                   c(min(nodes[,2])-1,max(nodes[,2])+1)
    ),
    spatstat::owin(c(min(features[,1])-1,max(features[,1])+1),
                   c(min(features[,2])-1,max(features[,2])+1)
    )
  )) 
  return(result_bb)
}


#' Plotting Voronoi Tesselation with Amount of Features
#' 
#' @title plot_voro_fea
#' @param nodes a data.frame containing metric x and y coordinates of nodes, additionally an identifier. Coordinates are expected to be the first two columns.
#' @param df_aggr_fea a dataframe with feature types and their corresponding node
#' @param ppp_nd a ppp object of nodes
#' @param voronoi a tesselation list object generated from nodes
#'
#' @return an owin object which is slightly bigger, to avoid missing points due to edge effects
#' @author Juliane Watson <\email{juliane.bonness@@web.de}>

plot_voro_fea <- function(df_aggr_fea, nodes, ppp_nd, voronoi){
  
  count_fea <- dplyr::count(df_aggr_fea,df_aggr_fea[,2])
  colnames(count_fea) <-  c("ID", "count")
  for_plot <-  merge(nodes, count_fea, by.x="nodes_id",by.y="ID", all.x = TRUE)
  plot(voronoi, main = "Amount of features per node")
  text(ppp_nd, as.character(for_plot$count),col="red")
}