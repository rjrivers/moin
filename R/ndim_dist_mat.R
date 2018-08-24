#' n Dimensional Distance Matrix
#' 
#' Creates an n dimensional bidirectional distance matrix from a dataframe. If a measure of cultural distance is desired, the data should be structured with a node identifier, followed by the typespectra collums. 
#' 
#' @title dist.matr
#' 
#' @param type_spectra a dataframe containing node compositions
#' @param method character string, the distance measure to be 
#'   used ("euclidean", "maximum", "manhattan", "canberra", 
#'   "binary" or "minkowski")
#' 
#' @return symmetrical distance matrix
#' 
#' @author Oliver Nakoinz <\email{oliver.nakoinz@ufg.uni-kiel.de}>
#' @author Franziska Faupel <\email{franziska-faupel@gmx.de}>
#' @author Georg Roth <\email{georg.roth@fu-berlin.de}>
#'
#' @examples
#' set.seed(1234)
#' 
#' nodes <- data.frame(nodes_x = sample(3433806:3581396, 10, replace = TRUE),
#'                nodes_y = sample(5286004:5484972, 10, replace = TRUE), 
#'                nodes_id = c(1:10))
#' features <- data.frame(x = sample(3433806:3581396, 100, replace = TRUE),
#'                    y = sample(5286004:5484972, 100, replace = TRUE),
#'                    type = paste0("B", c(rep(1, 5), rep(2,15), sample(11:19, 20, replace = TRUE), sample(111:119, 30, replace = TRUE), sample(1111:1115, 30, replace = TRUE)))
#'                    ) 
#' aggr_fea <- aggr_feature_voro(nodes, features, "type")
#' typelist <- create_type_generator(features, "type", 1)
#' 
#' type_spectra <- create_typespectra(aggr_fea, typelist) 
#' 
#' distancematrix <- dist_matr(type_spectra, method = "euclidean")
#' 
#' 
#' @export

dist_matr <- function(type_spectra, method = "euclidean"){
    type_dist_mat <- as.matrix(
      dist(as.matrix(
        type_spectra[,2:ncol(type_spectra)]),
        diag=TRUE, upper=TRUE) )

    rownames(type_dist_mat) <- colnames(type_dist_mat) <- type_spectra[,1]
    return(type_dist_mat)
}

