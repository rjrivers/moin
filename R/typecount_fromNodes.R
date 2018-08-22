#test
import_stuff = function(csv = "Data/shkr-weapons.csv"){
    data = read.csv(csv, sep = ";")
    ti_data = tibble::as_tibble(data)
    return(ti_data)
}



#ALL ABOVE CAN GO


##TODO add input variable if runtrhough is wanted 
create_empty_typelist <- function(typelist){
    data = 0
    data = tibble::as.tibble(data)
    data[, typelist] = 0 
    colnames(data) = c("node_id", typelist)
    return(data)
}

estlim = create_type_generator(type_tibble = import_stuff(), column = "loc10_typ_b", pre_size = 1)


csv2 = "Data/reslut.csv"
datares = read.csv(csv2, sep = ",")
 


create_typesectrum_for_sties <- function(node_id, node_type, list_to_modify){
    list_to_modify = list_to_modify
    typelist_node = mtypes(node_type, 1)
    indexes = c()
    
    for (element in typelist_node) {
        which = which(colnames(list_to_modify) == element)
        indexes = c(indexes, which) }
    
    row = which(list_to_modify$node_id == node_id)
    list_to_modify[row, indexes] = list_to_modify[row, indexes] + 1
        
    return(list_to_modify)
}

endlist = df

for (e in 1:nrow(new_result)) {
    endlist = create_typesectrum_for_sties(new_result[e,2], new_result[e,1], endlist)
}
VieView(endlist)
