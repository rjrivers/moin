#' Cultural distance matrix
#' Creates a cultural distance matrix by aggregating all features to the nodes by means of a Voronoi tesselation. xxxxxxx. The n dimensional distance matrix of the nodes and their assigned features will be calculated with euclidean distances. The bidirectional matrix will be returned and can be used as network weight.
#' @title cul.dist
#' @param nodes_x a vector containing metric x coordinates of nodes
#' @param nodes_y a vector containing metric y coordinates of nodes
#' @param nodes_id a vector containing ID for nodes
#' @param features a data.frame containing metric x and y coordinates of features, and feature type. Coordinates are expected to be the first two columns.
#' @param type_col a character string naming the columname containing feature types.
#' @param pre_size numeric, amount of letters, e.g. characters before typenumbers
#' @param method character string, the distance measure to be 
#'   used ("euclidean", "maximum", "manhattan", "canberra", 
#'   "binary" or "minkowski")
#' 
#' @return bidirectional cultural distance matrix 
#' @export
#' @author Franziska Faupel <franziska-faupel@gmx.de>
#'
#' @examples
#'

cul_dist <- function(nodes_x, nodes_y,nodes_id,features,type_col, pre_size=1, method){
    nodes <- data.frame(nodes_x, nodes_y, nodes_id)
    aggr_fea <- aggr_fea_voro(nodes,features, type_col)
    typelist <- create_type_generator(features, type_col, pre_size)
    type_spectra <- create_typespectra(aggr_fea, typelist)
    cul_dist <- dist_matr(type_spectra,method)
    return(cul_dist)
}