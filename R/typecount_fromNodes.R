#' @title create empty type list
#' @param typelist create.type.generator
#' @param aggr_fea generated nodelist with types as output from arrg_fea.aggr.fea.voro()
#' @examples some example 
#' @export

create_empty_typelist <- function(typelist, aggr_fea){
    nodes <- unique(aggr_fea[, 2])
    data <- matrix(nrow = length(nodes), ncol = length(typelist) + 1, data = 0)
    data[, 1] <- sort(unique(aggr_fea[, 2]))
    data <- as.data.frame(data)
    colnames(data) <- c("node_id", typelist)
    return(data)
}


#' @title create typespectra for nodes
#' @param node_id id of a single node (line[x,2] from output)
#' @param node_type type found on a single node (line[x,1] from output)
#' @examples some example 
#' @export 

create_typesectra_for_nodes <- function(node_id, node_type, list_to_modify){
    list_to_modify <- list_to_modify
    typelist_node <- mtypes(node_type, 1)
    indexes <- c()
    
    for (element in typelist_node) {
        which <- which(colnames(list_to_modify) == element)
        indexes <- c(indexes, which) }
    
    row <- which(list_to_modify$node_id == node_id)
    list_to_modify[row, indexes] <- list_to_modify[row, indexes] + 1
        
    return(list_to_modify)
}


#' @title create_typespectra
#' @param aggr_fea generated nodelist with types as output from arrg_fea.aggr_fea_voro()
#' @param typelist create_type_generator output
#' @examples some example 
#' @export

create_typespectra <- function(aggr_fea, typelist){
    export <- create_empty_typelist(typelist, aggr_fea)
    output <- export
    for (i in 1:nrow(aggr_fea)) {
        output <- create_typesectra_for_nodes(aggr_fea[i,2], aggr_fea[i,1], output)
    }
    return(output)
}

