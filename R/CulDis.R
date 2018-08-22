#' Cultural distance matrix
#' Creates a cultural distance matrix by aggregating all features to the nodes by means of a Voronoi tesselation. xxxxxxx. The n dimensional distance matrix of the nodes and their assigned features will be calculated with euclidean distances. The bidirectional matrix will be returned and can be used as network weight.
#' @title cul.dist
#' @param nodes a data.frame containing metric x and y coordinates of nodes, additionally an identifier. Coordinates are expected to be the first two columns.
#' @param features a data.frame containing metric x and y coordinates of nodes, and feature type. Coordinates are expected to be the first two columns.
#'
#' @return bidirectional cultural distance matrix 
#' @export
#'
#' @examples
#'

cul.dist <- function(nodes,features){
    aggr_fea <- aggr.fea.voro(nodes,features)
    # type list
    # type spectra return -> xxx
    cul_dist <- dist.matr(xxx)
    return(cul_dist)
}