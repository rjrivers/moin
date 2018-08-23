#' n Dimensional Euclidian Distance Function
#' Calculates the euclidian distance of the compositions of node A and B. If a measure of cultural distance is desired, the composition equals the type spectra. 
#' @titel edist
#' @param a vector containing the compositions of node A. 
#' @param b vector containing the compositions of node B.
#'
#' @return euclidian distance between node A and node B
#' @export
#'
#' @examples
#' 

edist <- function(a,b){sqrt(sum((a-b) ^ 2))}

#' n Dimensional Distance Matrix
#' Creates an n dimensional bidirectional distance matrix from a dataframe. If a measure of cultural distance is desired, the data should be structured with a node identifier, followed by the typespectra collums. 
#' @title dist.matr
#' @param df a dataframe containing node compositions
#' @return bidirectional distance matrix
#' @export
#'
#' @examples

dist_matr <- function(df){
    l <- length(df[,1])
    con_all <- matrix(data = NA, nrow = l, ncol = l)
    for(i in 1:(l-1)){
        a <- df[i,-1]
            for(j in ((i+1):l)){
                b <- df[j,-1]
                dis <- edist(a,b)
                con_all[i,j] <- dis
                con_all[j,i] <- dis
            }
        con_all[i,i] <- 0
        con_all[l,l] <- 0
    }
    return(con_all)
}
