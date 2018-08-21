
pre_size <- 1   # prefix size
type <- "A132543"


#' This function extracts the meta type-strings of a type string
#'
#' @param type A string containing the type code
#' @param pre_size A number of the lenght of the type prefix such as "Type_" in "Type_53234"
#'
#' @return A vector of meta types of the original type
#' @export
#'
#' @examples
#' mtypes("Type_53234", 5)
mtypes <- function(type, pre_size) {
    type_length <- nchar(type)
    parts <- type_length - pre_size
    metatypes <- 1:parts
    for (i in 1:parts){
        metatypes[i] <- substr(type, 1, pre_size + i)
    }
    return(metatypes)
}


# mtypes(type, pre_size)


# Complete a list of types with metatypes

pre_size <- 1   # prefix size
type_list <- c("A132543", "A232543", "A132542", "A134543")



#' Type list completion
#'
#' @param type_list A vector of strings of type codes
#' @param pre_size A number of the lenght of the type prefix such as "Type_" in "Type_53234"
#'
#' @return A vector of strings of type codes. In contrast to the input type list "type_list" this vector contains all meta-types which are represented by types in the input type-list
#' @export
#'
#' @examples comptypelist(type_list = c("A132543", "A232543", "A132542", "A134543"), pre_size = 1)
comptypelist <- function(type_list, pre_size){
    type_list <- type_list[!is.na(type_list)]
    typelistlist <- lapply(type_list, mtypes, pre_size = pre_size)
    typelist <- unlist(typelistlist)
    typelist <- sort(typelist)
    typelist <- unique(typelist)
    return(typelist)
}


comptypelist(type_list, pre_size)


# Teile A221A21A4 

type_list <- c("A132543", "A232A543", "A1325A42", "A1A34543")
prefix <- "A"

#' Split concatenated type codes in one string
#'
#' @param typevector A vector of type characters, partly concatenated in one string and separated by prefix
#' @param prefix A string of the prefix of the type-code
#'
#' @return A vector of type characters without concatenated types in one string
#' @export
#'
#' @examples splittypes(c("A132543", "A232A543", "A1325A42", "A1A34543"), "A")
splittypes <- function(typevector, prefix){
    typevector2 <- c()
    separator <- paste("[", prefix, "]", sep = "")
    for (i in 1:length(typevector)){
        x <- unlist(strsplit(typevector[i], separator))
        x <- paste("A", x[-1], sep = "")
        typevector2 <- c(typevector2, x)
    }
    return(typevector2)
}



splittypes(type_list, prefix)


