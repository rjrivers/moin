
testdata <- sf::st_read("../vignettes/data/OldenburgerGraben/Placenames.shp") 

# MDM - Maximum Distance Modell
mdm <- modelgraph(testdata,
                  "mdm",
                  4000)

spatgraph_transform(x=mdm,output="tidygraph")

# PPA - Proximal Point Analysis
ppa <- modelgraph(testdata,
                  "ppa",
                  5)

spatgraph_transform(x=ppa,output="tidygraph")




############

dista <- sf::st_distance(x = testdata, 
                         y = testdata)

dista2 <- as.dist(dista)

clustertree <- hclust(dista2, method = "single")
plot(clustertree, hang = -1) 

memb <- cutree(clustertree, h = 2500)

testdata$Membership <- memb

mapview::mapview(testdata["Membership"])


yxc <- sf::aggregate.sf(testdata,by=Membership)