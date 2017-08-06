MOdeling INteractions a package to calculate singly- and doubly-constrained interaction models
================
Daniel Knitter
2017-08-06

Introduction
============

For the moment this vignette shows only minimal examples on how to use the moin package. These are from textbooks presenting methods on singly- and doubly-constrained interaction models. The examples correspond to those from the help document to the functions. However, here also the expected results are shown.

Singly-constrained model -- A location model.
---------------------------------------------

\[
flows_{ij} = O_i \frac{W_j^\alpha c_{ij}^{-\beta}}{\sum_j W_j^\alpha c_{ij}^{-\beta}}
\]

where \(O_i\) are the sources of flows and \(W_j\) is the attractiveness of the target. It can be weighted by \(\alpha\). \(c_{ij}\) are the travel costs that can be weighted with \(\beta\).

Load the package

``` r
library(moin)
```

Create some data. These stem from Wilson and Kirkby (1980), p. 100f.

-   `Pi` is the population of zone i
-   `ei` is the mean expenditure on shopping goods per head in zone i
-   `Wj` is the attractiveness of shops in zone j
-   `cij` is the cost of travel from i to j

``` r
ei <- c(2,1,1)
Pi <- c(50, 1000, 500)
Wj <- c(10, 100, 20)
cij <- matrix(data = c(1, 5, 5,
                      5, 2.585, 5,
                      5, 5, 2),
              nr = 3,
              nc = 3
)
```

Apply the model

``` r
sc(Oi = ei * Pi, Wj = Wj, cij = cij, detfun = "power")
```

    ## $flows
    ##          [,1]      [,2]      [,3]
    ## [1,] 29.41176  58.82353  11.76471
    ## [2,] 44.75803 865.72591  89.51606
    ## [3,] 31.25000 312.50000 156.25000
    ## 
    ## $Dj
    ## [1]  105.4198 1237.0494  257.5308

Here are the original data. \(S_{11}\) to \(S_{33}\) correspond to the `flows` as returned by `sc` function. `Dj` corresponds to \(\sum_i S_{ij}\)

<img src="WilsonKirkby1980_p101.jpg" width="500px" style="display: block; margin: auto;" />

What is important here? The main characteristics is that the flow totals are constrained by \(e_i * P_i\). Due to this it can act as a location model and predict the spatial distribution of shopping sales (these are the `Dj` values).

Doubly-constrained model
------------------------

\[
T_{ij} = A_i O_i B_j D_j c_{ij}^{-\beta}
\]

with constraints

\[
\sum_j^n T_{ij} = O_i
\]

and

\[
\sum_i^n T_{ij} = D_j
\]

Therefore, two balancing scalars are necessary to ensure that the origin (\(A_i\)) and destination (\(B_j\)) constraints are satisfied.

\[
A_i = (\sum_j^n B_j D_j c_{ji}^{-\beta})^{-1}
\]

and

\[
B_j = (\sum_i^n A_i O_i c_{ji}^{-\beta})^{-1}
\]

Create some data. These stem from Thomas and Huggett (1980), p. 150. It is a journey-to-work example (see figure)

<img src="Thomas_Huggett_1980_150.jpg" width="700px" style="display: block; margin: auto;" />

`Oi` refers to workers in zone `i` and `Dj` to jobs in `j`.

``` r
Oi <- c(4,6,2)
Dj <- c(3,8,1)
cij <- matrix(data = c(1,2,2,
                       2,1,2,
                       2,2,1
                       ),
               nr = 3,
               nc = 3
               )
beta <- 1
```

Apply the model

``` r
tmp <- dc(Oi = Oi, Dj = Dj, cij = cij, iterations = 5)
str(tmp)
```

    ## List of 10
    ##  $ iteration: num 4
    ##  $ beta     : num 1
    ##  $ Oi       :'data.frame':   3 obs. of  2 variables:
    ##   ..$ Target: num [1:3] 4 6 2
    ##   ..$ sj    : num [1:3] 4 6 2
    ##  $ Dj       :'data.frame':   3 obs. of  2 variables:
    ##   ..$ Target: num [1:3] 3 8 1
    ##   ..$ si    : num [1:3] 3 8 1
    ##  $ Ratio    :'data.frame':   3 obs. of  2 variables:
    ##   ..$ rj: num [1:3] 1 1 1
    ##   ..$ ri: num [1:3] 0.75 1.33 0.5
    ##  $ error    : num 0.000348
    ##  $ Ai       :'data.frame':   4 obs. of  3 variables:
    ##   ..$ X1: num [1:4] 1 0.133 0.133 0.133
    ##   ..$ X2: num [1:4] 1 0.1 0.101 0.101
    ##   ..$ X3: num [1:4] 1 0.154 0.152 0.152
    ##  $ Bj       :'data.frame':   4 obs. of  3 variables:
    ##   ..$ X1: num [1:4] 1 1.01 1.02 1.02
    ##   ..$ X2: num [1:4] 1 0.98 0.979 0.979
    ##   ..$ X3: num [1:4] 1 1.14 1.15 1.15
    ##  $ Tij      : num [1:3, 1:3] 1.617 0.92 0.463 2.078 4.733 ...
    ##  $ sumTij   : num 12

Now, the model output is a little longer. `Oi` and `Dj` have Target and `si` or `sj` columns. The former are the input data, the latter the calculated values.

``` r
tmp$Ai
```

    ##          X1        X2        X3
    ## 1 1.0000000 1.0000000 1.0000000
    ## 2 0.1333333 0.1000000 0.1538462
    ## 3 0.1327950 0.1006996 0.1519117
    ## 4 0.1327341 0.1007419 0.1518597

``` r
tmp$Bj
```

    ##         X1        X2       X3
    ## 1 1.000000 1.0000000 1.000000
    ## 2 1.012987 0.9798995 1.143695
    ## 3 1.015032 0.9787616 1.147431
    ## 4 1.015206 0.9786849 1.147561

`Ai` and `Bj` refer to the balancing factors. Since these result from simultaneous equations they need to be find by iteration. `iteration` states that the model ran for four iterations, although we stated five explicitly in the model call. The reason is, that the model was balanced already after the fourth iteration. Compared to the values of Thomas and Huggett (1980), p. 151 it gets obvious the the values are rather different (this might be caused by rounding effects throughout the calculations).

<img src="Thomas_Huggett_1980_151.jpg" width="400px" style="display: block; margin: auto;" />

``` r
round(tmp$Tij, 2)
```

    ##      [,1] [,2] [,3]
    ## [1,] 1.62 2.08 0.30
    ## [2,] 0.92 4.73 0.35
    ## [3,] 0.46 1.19 0.35

`Tij` shows the flows and these correspond to those from Thomas and Huggett (1980), p. 152

<img src="Thomas_Huggett_1980_152.jpg" width="300px" style="display: block; margin: auto;" />

A more elaborate example
========================

A location model of settlement locations in SE Turkey

Load Data
---------

``` r
library(magrittr)
library(rgdal)
```

    ## Loading required package: sp

    ## rgdal: version: 1.2-8, (SVN revision 663)
    ##  Geospatial Data Abstraction Library extensions to R successfully loaded
    ##  Loaded GDAL runtime: GDAL 2.1.3, released 2017/20/01
    ##  Path to GDAL shared files: /usr/share/gdal/2.1
    ##  Loaded PROJ.4 runtime: Rel. 4.9.2, 08 September 2015, [PJ_VERSION: 492]
    ##  Path to PROJ.4 shared files: (autodetected)
    ##  Linking to sp version: 1.2-5

``` r
library(rgeos)
```

    ## rgeos version: 0.3-23, (SVN revision 546)
    ##  GEOS runtime version: 3.5.1-CAPI-1.9.1 r4246 
    ##  Linking to sp version: 1.2-5 
    ##  Polygon checking: TRUE

``` r
harran <- readOGR(dsn = "data",
                  layer = "harran_plain") %>%
  spTransform(CRSobj = "+init=epsg:32634")
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "data", layer: "harran_plain"
    ## with 1 features
    ## It has 1 fields
    ## Integer64 fields read as strings:  id

``` r
tellpoly <- readOGR(dsn = "data",
                    layer = "settlements") %>%
  spTransform(CRSobj = "+init=epsg:32634")
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "data", layer: "settlements"
    ## with 745 features
    ## It has 3 fields
    ## Integer64 fields read as strings:  id

``` r
tellpoly@data$area <- gArea(tellpoly, byid = TRUE)
tellpoly <- tellpoly[harran,]

tellpoints <- tellpoly %>%
  gCentroid(byid=TRUE) %>%
  SpatialPointsDataFrame(data = data.frame(x = .@coords[,1],
                                           y = .@coords[,2],
                                           Tell = tellpoly@data$Tell))

library(mapview)
```

    ## Loading required package: leaflet

``` r
## mapview(harran) + mapview(tellpoly) + mapview(tellpoints)
```

Create Delaunay Graph and calculate the distances between the connected points
------------------------------------------------------------------------------

``` r
library(spdep)
```

    ## Loading required package: Matrix

``` r
telldel <- tri2nb(coords = tellpoints@coords)
```

    ## 
    ##      PLEASE NOTE:  The components "delsgs" and "summary" of the
    ##  object returned by deldir() are now DATA FRAMES rather than
    ##  matrices (as they were prior to release 0.0-18).
    ##  See help("deldir").
    ##  
    ##      PLEASE NOTE: The process that deldir() uses for determining
    ##  duplicated points has changed from that used in version
    ##  0.0-9 of this package (and previously). See help("deldir").

``` r
distmat <- gDistance(tellpoints, byid = TRUE)
distmat <- distmat / 1000

for (i in 1:dim(distmat)[1]) {
  distmat[i,-c(telldel[[i]])] <- NA
  }
```

Interaction
-----------

``` r
library(moin)

int_mod <- sc(Oi = rep(x = 10, times = length(tellpoints)),
              Wj = rep(x = 5, times = length(tellpoints)),
              cij = distmat,
              beta = 1,
              detfun = "power")
tellpoints@data$Dj <- int_mod$Dj

## mapview(tellpoints,
##         cex = tellpoints$Dj,
##         zcol = "Dj")  
```

Network measures as attributes for Interaction
----------------------------------------------

``` r
g <- igraph::graph_from_adj_list(adjlist = telldel,
                                 mode = "all")
tellpoints@data$degree <- igraph::degree(g)
tellpoints@data$closeness <- igraph::closeness(g)
tellpoints@data$betweeness <- igraph::betweenness(g)

int_mod <- sc(Oi = rep(x = 10, times = length(tellpoints)),
              Wj = tellpoints@data$degree,
              cij = distmat,
              beta = 1,
              detfun = "power"
              )
tellpoints@data$Dj <- int_mod$Dj

## mapview(tellpoints,
##         cex = tellpoints$Dj,
##         zcol = "Dj")
```

### What is the influence of different beta values on the location importance of sites?

First, let us have a look at changing \(\beta\) values for the power function

``` r
beta_test <- data.frame(beta = 0,
                        Dj = 0,
                        degree = 0
                        )
beta_seq <- seq(from = 0.1, to = 2.0, by = .1)

for (i in 1:length(beta_seq)) {
  j <- beta_seq[i]
  beta_test <- rbind(beta_test, data.frame(beta = rep(j, length(tellpoints)),
                                           Dj = sc(Oi = rep(x = 10, times = length(tellpoints)),
                                                   Wj = tellpoints@data$degree,
                                                   cij = distmat,
                                                   beta = j,
                                                   detfun = "power"
                                                   )$Dj,
                                           degree = tellpoints$degree)
                     )
}
beta_test <- beta_test[2:length(beta_test[,1]),]
head(beta_test)
```

    ##     beta        Dj degree
    ## 76   0.1  9.667618      6
    ## 77   0.1  5.822126      5
    ## 94   0.1  8.141451      5
    ## 96   0.1  7.684073      5
    ## 97   0.1  8.763060      5
    ## 143  0.1 12.370666      7

``` r
beta_test2 <- beta_test %>%
  dplyr::as.tbl()

library(ggplot2)
ggplot(beta_test2, aes(x = Dj, y = as.factor(beta), fill = as.factor(degree))) + 
  ggjoy::geom_joy(scale = 2.5, rel_min_height=.01, alpha = .8, color = "white") +
  ggjoy::theme_joy(font_size = 10) +
  theme(legend.position = "bottom") +
  viridis::scale_fill_viridis(discrete=TRUE,
                              name = "Degree centrality") +
  labs(x = "si",
       y = expression(paste(beta," factor of power function")),
       title = "Location attractiveness (Dj) in relation to degree-centrality and distance weight (beta) factors",
       subtitle = "Connections based on Delaunay graph; Origins weighted equally; attractiveness equals degree centrality",
       caption = "Data source: settlements in Harran Ovası, SE Turkey, digitized from Corona satellite images (acquisition dates ~1960-1970)") +   
  guides(fill=guide_legend(nrow = 1))
```

    ## Picking joint bandwidth of 0.537

<img src="Modelling_Spatial_Interactions_files/figure-markdown_github-ascii_identifiers/eex_tests-1.png" style="display: block; margin: auto;" />

References
==========

Thomas, Reginald William, and Richard J. Huggett. 1980. *Modelling in Geography: A Mathematical Approach*. Rowman & Littlefield.

Wilson, Alan Geoffrey, and Michael John Kirkby. 1980. *Mathematics for Geographers and Planners*. 2nd ed. Contemporary Problems in Geography 1. Oxford: Clarendon Pr.
