#create_type_generator muss aufgerfudne werdne = complete_types
#input von kirara + Oliver


#'@title join area types
#'@param areas areas of nodes 
#'@param types complete types from create_type_generatro
#'@examples 
#'@export 

join_area_types = function(areas, types){
    # find id
    # join types with id (all) as col
}

find_existing_types = function(areas, area_compTypes){
    #build 
}


#'@title create node typelist
#'@param areas areas of nodes 
#'@param types complete types from create_type_generatro
#'@examples 
#'@export 

create_node_typelist = function(areas, types){
    area_complete_types = join_area_types(areas, types)
    area_existing_types = find_existing_types(area_complete_types)
}