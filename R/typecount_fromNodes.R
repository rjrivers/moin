#test
import_stuff <- function(csv = "Data/shkr-weapons.csv"){
    data <- read.csv(csv, sep = ";")
    ti_data <- tibble::as_tibble(data)
    return(ti_data)
}
result_from_nodes <- read.csv(csv2, sep = ",")

##TODO add input variable for output

#'@title create empty type list
#'@param typelist create.type.generator
#'@param input generated nodelist with types as output from arrg_fea.aggr.fea.voro()
#'@examples some example 
#'@export

create.empty.typelist <- function(typelist, input){
    nodes <- unique(input[, 2])
    data <- matrix(nrow = length(nodes), ncol = length(typelist) + 1, data = 0)
    data[, 1] <- sort(unique(input[, 2]))
    data <- as.data.frame(data)
    colnames(data) <- c("node_id", typelist)
    return(data)
}

test_typelist <- create.type.generator(type_tibble = import_stuff(), column = "type", pre_size = 1)
test_export <- create.empty.typelist(test_typelist, result_from_nodes)
output <- test_export

""
#ALL ABOVE CAN GO UNLESS WE STILL NEED CREATE
""

#'@title create typespectrum for node
#'@param node_id id of a single node (line[x,2] from output)
#'@param node_type type found on a single node (line[x,1] from output)
#'@examples some example 
#'@export 

create.typesectrum.for.nodes <- function(node_id, node_type, list_to_modify){
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

#TODO decide what to do. either in other function or in wrapper
for (i in 1:nrow(result_from_nodes)) {
    output <- create.typesectrum.for.nodes(result_from_nodes[i,2], result_from_nodes[i,1], output)
}

