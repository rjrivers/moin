# id = loc07_id test = 3156
# type = loc10_typ_b
# anzahl pro typ: fundzahl

import = import_stuff()
id = 3156
subs = import[import$loc07_id == id,]
type = "B413"
su_type = subs[subs$loc10_typ_b == type,]


add_typeamount_to_table <- function(original_element, id, type, column_amount, list_to_modify, first){
    index_amount = grep(column_amount, colnames(original_element))
    amount = su_type[, index_amount]
    index_type = grep(type, colnames(list_to_modify))
    row = check_id_row(id, list_to_modify)
    list_to_modify[row,index_type] = testdata[row,index_type] + amount
    return(list_to_modify)
}

check_id_row <- function(id, data){
    data_id <- data$id
    
     if (id %in% data_id == FALSE) {
        row_data = nrow(data) + 1
        data[row_data, ] <- c(id, sample(0:0, length(data)-1, replace= T))
        return(row_data)
     } 
    value = which(data$id == id)
    return(value)
    }

create_typesectrum_for_sties(original_element, id, type, column_amount, list_to_modify){
    typelist_site = sort(mtypes(type, 1), decreasing = T)
    typespectrum = add_typeamount_to_table(element, id, type, column_amount, list_to_modify, first = T)
    typelist_site = typelist_site[2:length(typelist_site)]
    for (element in typelist_site) {
        #TODO add asw. for T and F
        typespectrum = add_typeamount_to_table(element, id, type, column_amount, list_to_modify, first = F)
    }
}

newlist = add_typeamount_to_table(su_type, 3156, type, "fundzahl", testdata)
