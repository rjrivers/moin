#'@title parse all types
#'@param typelist a tibble with type information (eg. exampledata R1234)
#'@examples returns flat list of types
#'@export 

parse_all_types = function(type_tibble, ind){
    typelist = type_tibble[, ind]
    View(typelist)
    tyli = as.list(typelist)
    tyli = unlist(tyli)
    return(tyli)
}

#'@title mtypes
#'@param type one type from list
#'@examples returns flat list of meta types
#'@export 

mtypes <- function(type, pre_size) {
    type = as.character(type)
    type_length <- nchar(type)
    parts <- type_length - pre_size
    metatypes <- 1:parts
    for (i in 1:parts){
        metatypes[i] <- substr(type, 1, pre_size + i)
    }
    return(metatypes)
}


#'@title find missing types
#'@param typelist a list with type information (eg. exampledata R1234)
#'@examples returns flat list of types
#'@export 
#'

find_missing_types <- function(type_list, pre_size){
    type_list <- type_list[!is.na(type_list)]
    typelistlist <- lapply(type_list, mtypes, pre_size = pre_size)
    typelist <- unlist(typelistlist)
    typelist <- sort(typelist)
    typelist <- unique(typelist)
    return(typelist)
}



#'@title create type generator
#'@param type_tibble a tibble with type information (eg. exampledata R1234)
#'@examples 
#'@export 

create_type_generator = function(type_tibble, column){
    index = grep(column, colnames(type_tibble))
    print(index)
    list_of_types = parse_all_types(type_tibble, index)
    print(list_of_types)
    complete_types = find_missing_types(list_of_types, 1)
    print(complete_types)
    print("Done with creating type tibble")
    return(complete_types)
}

#test
import_stuff = function(csv = "Data/shkr-weapons.csv"){
    data = read.csv(csv, sep = ";")
    ti_data = tibble::as_tibble(data)
    return(ti_data)
}

import = import_stuff()
importtyli = create_type_generator(import_stuff(), "loc10_typ_b")
