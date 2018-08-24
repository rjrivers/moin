#' @title parse all types
#' 
#' Function which returns a data frame containing type of each feature. 
#' 
#' @param typelist a tibble with type information (eg. exampledata R1234)
#' @param indenx_col index for column to use
#' @return returns a data frame containg all features
#' @author Juliane Watson <juliane.bonness@web.de>

parse_all_types <- function(type_tibble, index_col){
    typelist <- type_tibble[, index_col]
    typelist <- as.list(typelist)
    typelist <- unlist(typelist)
    return(typelist)
}


#' @title mtypes
#' 
#' Separating hierarchical type names, so upper levels will be returned. 
#'   R123 will be returned as R1, R12, R123.
#' 
#' @param type one type from list e.g. R12345
#' @param pre_size amount of letters e.g. charactes before typenumber
#' @return returns upper level of type hierarchy as list
#' @examples returns flat list of meta types
#' @author Oliver Nakoinz <oliver.nakoinz@ufg.uni-kiel.de>

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


#' @title find missing types
#' 
#' Sanity check if each upper level of hierarchical types is represented once in `typelist`.
#' 
#' @param typelist a list with type information (eg. exampledata R1234)
#' @param pre_size amount of letters e.g. charactes before typenumber
#' @return returns flat list of types
#' @examples 
#' @author Juliane Watson <juliane.bonness@web.de>


find_missing_types <- function(type_list, pre_size){
    type_list <- type_list[!is.na(type_list)]
    typelistlist <- lapply(type_list, mtypes, pre_size = pre_size)
    typelist <- unlist(typelistlist)
    typelist <- sort(typelist)
    typelist <- unique(typelist)
    return(typelist)
}


#' @title create type generator
#' @param type_tibble a tibble with type information (eg. exampledata R1234)
#' @param type_col string for column with type information 
#' @param pre_size amount of letters e.g. charactes before typenumber
#' @examples some example 
#' @export 
#' @author Juliane Watson <juliane.bonness@web.de>

create_type_generator <- function(type_tibble, type_col, pre_size){
    index_col <- which(colnames(type_tibble) == type_col)
    list_of_types <- parse_all_types(type_tibble, index_col)
    complete_types <- find_missing_types(list_of_types, pre_size)
    print("Done with creating type list")
    return(complete_types)
}

