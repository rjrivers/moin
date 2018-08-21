#'@title test function
#'@param nodes nodes of a graph with types 
#'@param edges edges of a graph 
#'@examples 
#'@export 

test_function = function(name = "Julie"){
  '%&%' <- function(x, y)paste0(x,y)
  print(paste("Hello", name %&% "!!!"))
}


#'@title parse all types
#'@param typelist a tibble with type information (eg. exampledata R1234)
#'@examples returns flat list of types
#'@export 

parse_all_types = function(type_tibble){
  #find all types
  #make list
  return(typelist)
}


#'@title find missing types
#'@param typelist a list with type information (eg. exampledata R1234)
#'@examples returns flat list of types
#'@export 

find_missing_types = function(typelist){
  #check structure eg R1234 R123 R12 R1
  #subset max stellen 
    #--> for every max-1
  #compare with 2 (e.g. R1 missing)
  #make list
  return(typelist_missing)
}


#'@title tidy tibble list
#'@param typelist_missing a list with complete type information (returned from find_missing_types)
#'@examples returns flat list of types
#'@export

tidy_tibble_list = function(typelist_missing){
    #make list tibble
    #tidy
    return(tidy_tibble)
}


#'@title tidy tibble list
#'@param typelist_missing a list with complete type information (returned from find_missing_types)
#'@examples returns flat list of types
#'@export

tidy_tibble_list = function(typelist_missing){
    #make list tibble
    #tidy
    return(tidy_tibble)
}


#'@title create type generator
#'@param type_tibble a tibble with type information (eg. exampledata R1234)
#'@examples 
#'@export 

create_type_generator = function(type_tibble){
  list_of_types = parse_all_types(type_tibble)
  missing_types = detect_missing_types(list_of_types)
  complete_types = tidy_tibble_list(missing_types)
  print("Done with creating type tibble")
  return(complete_types)
}

