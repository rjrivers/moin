#'@title parse all types
#'@param typelist a tibble with type information (eg. exampledata R1234)
#'@param ind index for column to use
#'@return returns flat list of types
#'@export 

parse_all_types <- function(type_tibble, ind){
    typelist <- type_tibble[, ind]
    typelist <- as.list(typelist)
    typelist <- unlist(typelist)
    return(typelist)
}

#'@title mtypes
#'@param type one type from list e.g. R12345
#'@examples returns flat list of meta types
#'@export 

mtypes <- function(type, pre_size) {
    type <- as.character(type)
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
#'@param column string for column with type information 
#'@examples some example 
#'@export 

create_type_generator <- function(type_tibble, column){
    index <- grep(column, colnames(type_tibble))
    list_of_types <- parse_all_types(type_tibble, index)
    complete_types <- find_missing_types(list_of_types, 1)
    print("Done with creating type list")
    return(complete_types)
}

