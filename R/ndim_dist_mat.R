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
#' 
#' @export

dist_matr <- function(type_spectra, method="euclidean"){
    type_dist_mat <- as.matrix(
      dist(as.matrix(
        type_spectra[,2:ncol(type_spectra)]),
        diag=TRUE, upper=TRUE) )

    rownames(type_dist_mat) <- colnames(type_dist_mat) <- type_spectra[,1]
    return(type_dist_mat)
}

