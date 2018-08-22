#tibble wie 


#test
import_stuff = function(csv = "Data/shkr-weapons.csv"){
    data = read.csv(csv, sep = ";")
    ti_data = tibble::as_tibble(data)
    return(ti_data)
}

typelist_comp = create_type_generator(type_tibble = import_stuff(), column = "loc10_typ_b", pre_size = 1)



# id = loc07_id test = 3156
# type = loc10_typ_b
# anzahl pro typ: fundzahl
testdata = 0
testdata = tibble::as.tibble(testdata)
testdata[, typelist_comp] = 0 
colnames(testdata) = c("id", typelist_comp)

import = import_stuff()
id = 3156
subs = import[import$loc07_id == id,]
type = "B413"
su_type = subs[subs$loc10_typ_b == type,]


add_typeamount_to_table <- function(original_element, id, type, column_amount, list_to_modify){
    index_amount = grep(column_amount, colnames(original_element))
    amount = su_type[, index_amount]
    index_type = grep(type, colnames(testdata))
    row = nrow(list_to_modify+1)
    list_to_modify[row,index_type] = testdata[row,index_type] + amount
    return(list_to_modify)
}

check_id_row <- function(id, data){
    
    data_id <- data$id
    
     if (id %in% data_id == FALSE) {
        row_data = nrow(data) + 1
        data[row_data, ] <- c(id, sample(0:0, length(data)-1, replace= T))
        
        print("added new line")
        return(data)
     } else {  print("did not need to add new lin, id already exists.") }
    
    return(data)
    }

create_typesectrum_for_sties(original_element, id, type, column_amount, list_to_modify){
    typelist_site = mtypes(type, 1)
    for (elment in typelist_site) {
        typespectrum = add_typeamount_to_table(original_element, id, type, column_amount, list_to_modify)
    }
}

newlist = add_typeamount_to_table(su_type, 3156, type, "fundzahl", testdata)
