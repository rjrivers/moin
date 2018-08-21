#tibble wie 


#test
import_stuff = function(csv = "Data/shkr-weapons.csv"){
    data = read.csv(csv, sep = ";")
    ti_data = tibble::as_tibble(data)
    return(ti_data)
}

typelist_comp = create_type_generator(import_stuff(), "loc10_typ_b")

testdata = NA
testdata[, typelist_comp] = NA 
testdata[,1]
