#' @title create empty type list
#' @param typelist create.type.generator
#' @param aggr_fea generated nodelist with types as output from arrg_fea.aggr.fea.voro()
#' @author Juliane Watson <<\email{juliane.bonness@@web.de}>>

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
#' @author Juliane Watson <<\email{juliane.bonness@@web.de}>>

create_typesectra_for_nodes <- function(node_id, node_type, list_to_modify){
    list_to_modify <- list_to_modify
    typelist_node <- mtypes(node_type, 1)
    indexes <- c()
    
    for (element in typelist_node) {
        sel <- which(colnames(list_to_modify) == element)
        indexes <- c(indexes, sel) }
    
    row <- which(list_to_modify$node_id == node_id)
    list_to_modify[row, indexes] <- list_to_modify[row, indexes] + 1
        
    return(list_to_modify)
}


#' @title create_typespectra
#' 
#' @param aggr_fea generated nodelist with types as output from arrg_fea.aggr_fea_voro()
#' @param typelist create_type_generator output
#' 
#' @examples 
#' set.seed(1234)
#' 
#' nodes <- data.frame(nodes_x = sample(3433806:3581396, 10, replace = TRUE),
#'                nodes_y = sample(5286004:5484972, 10, replace = TRUE), 
#'                nodes_id = c(1:10))
#' features <- data.frame(x = sample(3433806:3581396, 100, replace = TRUE),
#'                    y = sample(5286004:5484972, 100, replace = TRUE),
#'                    type = paste0("B", c(rep(1, 5), rep(2,15), sample(11:19, 20, replace = TRUE), 
#'                    sample(111:119, 30, replace = TRUE), sample(1111:1115, 30, replace = TRUE)))
#'                    ) 
#' aggr_fea <- aggr_feature_voro(nodes, features, "type")
#' typelist <- create_type_generator(features, "type", 1)
#' 
#' create_typespectra(aggr_fea, typelist) 
#' 
#' @author Juliane Watson <<\email{juliane.bonness@@web.de}>
#' @export

create_typespectra <- function(aggr_fea, typelist){
    export <- create_empty_typelist(typelist, aggr_fea)
    output <- export
    for (i in 1:nrow(aggr_fea)) {
        output <- create_typesectra_for_nodes(aggr_fea[i,"nodes"], aggr_fea[i,"type"], output)
    }
    return(output)
}

