#' Cultural distance Network
#' 
#' Creates a network based on a cultural distance matrix 
#' 
#' @title moin_cult_dist
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
#' @param plotted a Boolean operator defining whether a plot should be created. Defaults to FALSE. Edge widths are scaled by maximum distance values and enlarged by factor 2. 
#' 
#' @return a list containing a graph object of classes tidygraph ("tbl_graph") resp. igraph ("igraph") and the cultural distance matrix.
#'         NOTE: The output igraph object contains zeros for display purpose. The cultural distance matrix (cult_dist_matr) is correct, containing NA for missing values.  
#'
#' @author Wolfgang Hamer <\email{hamer@@geographie.uni-kiel.de}>
#' @author Chiara Girotto <\email{chiara.girotto@@web.de}>
#' @author Hendrik Raese <\email{h.raese@@ufg.uni-kiel.de}>
#' @author Georg Roth <\email{georg.roth@@fu-berlin.de}>
#' 
#' @examples
#' 
#' set.seed(1234)
#' 
#' moin_cult_dist(nodes_x=sample(3433806:3581396, 10, replace = TRUE),
#'                nodes_y=sample(5286004:5484972, 10, replace = TRUE), 
#'                nodes_id=c(1:10), 
#'              features=data.frame(x=sample(3433806:3581396, 100, replace = TRUE),
#'                    y=sample(5286004:5484972, 100, replace = TRUE),
#'                    type=paste0("B", c(rep(1, 5), rep(2,15), sample(11:19, 20, replace = TRUE), 
#'                    sample(111:119, 30, replace = TRUE), sample(1111:1115, 30, replace = TRUE)))
#'                    ), 
#'              type_col="type" , pre_size=1, method = "euclidean", plotted = TRUE)
#'              )
#'
#' @export 


moin_cult_dist <- function(nodes_x,nodes_y, nodes_id, features, type_col,pre_size=1, method = "euclidean", plotted = FALSE){
    cudist <- cul_dist(nodes_x = nodes_x,
                       nodes_y = nodes_y,
                       nodes_id = nodes_id,
                       features = features,
                       method = method,
                       type_col = type_col,
                       pre_size = pre_size)
    missingnumb <- nodes_id[!is.element(nodes_id,rownames(cudist))]
    cult_dist <- cudist

    cudist <- rbind(cudist,matrix(0,nrow=length(missingnumb),ncol=nrow(cudist)))
    cudist <- cbind(cudist,matrix(0,ncol=length(missingnumb),nrow=nrow(cudist)))
    
    rownames(cudist) <- c(rownames(cudist)[1:(dim(cudist)[1]-length(missingnumb))], missingnumb)
    colnames(cudist) <- c(colnames(cudist)[1:(dim(cudist)[1]-length(missingnumb))], missingnumb)
    
    cudist <- cudist[order(as.numeric(rownames(cudist))),]
    cudist <- cudist[,order(as.numeric(colnames(cudist)))]
    
    net <- igraph::graph_from_adjacency_matrix(cudist, mode = "undirected", weighted = TRUE, diag = FALSE)
    igraph::V(net)$label = colnames(cudist)
    
    if(plotted){
        plot(net,layout=as.matrix(data.frame(x=nodes_x,y=nodes_y)), 
             edge.width = (igraph::E(net)$weight/max(igraph::E(net)$weight))*2,
             main = "Cultural Distance Network")
    }
    
    tidygraphobj <- tidygraph::as_tbl_graph(net)
    
    cudist <- rbind(cult_dist,matrix(NA,nrow=length(missingnumb),ncol=nrow(cult_dist)))
    cudist <- cbind(cudist,matrix(NA,ncol=length(missingnumb),nrow=nrow(cudist)))
    
    rownames(cudist) <- c(rownames(cudist)[1:(dim(cudist)[1]-length(missingnumb))], missingnumb)
    colnames(cudist) <- c(colnames(cudist)[1:(dim(cudist)[1]-length(missingnumb))], missingnumb)
    
    cudist <- cudist[order(as.numeric(rownames(cudist))),]
    cudist <- cudist[,order(as.numeric(colnames(cudist)))]
    
    
    return(list(tidygraph = tidygraphobj, cult_dist_matr = cudist))
}

