
################################################
## GSHDL Course: Introduction to R
## =============================================
## Project: Mosaic
## Author: Oliver Nakoinz
## Version: 01
## Date of last changes: 07.09.2016
## Data: a) HEK rings, b) monuments Dänischer Wohld, c)SRTM
## Author of data: a) (Nakoinz 2005), b) (Nakonz/Knitter 2016), c) Nasa/USGS
## Purpose: didactic
## Content:  ...
## Description: Code from the presentation
## Licence data: a) GPL, b) GPL, c) public domain
## Licence Script: GPL
##    (http://www.gnu.org/licenses/gpl-3.0.html)
################################################


wd <- "/home/fon/daten/analyse/mosaic"  
setwd(wd)  

df.weapons <- read.csv("2data/shkr-weapons.csv", header=TRUE, sep=";")
t  <- as.character(df.weapons[,13])
df.weapons <- data.frame(id=df.weapons[,4], x=df.weapons[,1], y=df.weapons[,2], t, stringsAsFactors = FALSE)

str(df.weapons)

df.weapons[1:4,]

tab.weapons <- table(df.weapons[,4])
tw <- tab.weapons
for (i in seq_along(tab.weapons)) {
    a <- dimnames(tab.weapons)[[1]][i]
    b <- dimnames(tab.weapons)[[1]][]
    tw[i] <- sum(tab.weapons[grepl(a, b)])
}

barplot(tw)

library(sp)                 # spatial objects
library(proj4)              # projection
library(spatstat)           # spatial statistics
library(maptools)           # spatial tools

crs1 <- "+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=WGS84 +units=m +no_defs" # gk3
coordinates(df.weapons)=~x+y    
proj4string(df.weapons) <- CRS(as.character(crs1)) 
str(df.weapons)

type.list <- unlist(dimnames(tab.weapons))
type.n <- length(type.list)

sdev <- 5000
bb   <- bbox(df.weapons)    
win  <- owin(xrange=c(bb[1,1],bb[1,2]), yrange= c(bb[2,1],bb[2,2]), unitname="m")
s.points    <- spsample(df.weapons, 100,  type="regular")

samp <- list() 
i=0
for (t in type.list) {
    i <- i+1
    finds <- df.weapons[which(df.weapons@data$t==t),]
    ppp_w <-  ppp(finds@coords[,1], finds@coords[,2], window=win)  
    dens <- density(ppp_w, kernel="gaussian", sigma=sdev, dimyx=c(36,56), w=win,  edge=TRUE, at="pixels")
    sgdf_w_dens   <- as.SpatialGridDataFrame.im(dens)
    proj4string(sgdf_w_dens) <- CRS(as.character(crs1))
    meg_dens_samp   <- over(s.points,  sgdf_w_dens)
    samp[[i]] <- meg_dens_samp
}

str(samp)
samp            # list of Typenspektren

ts <- data.frame(matrix(unlist(samp), nrow=99, byrow=T))   # points rows and types columns
colnames(ts) <- type.list
ts[1:4,1:3]

p.index <- (s.points@coords[,1] > 3503000 & s.points@coords[,1] < 3508000)
p.points <- s.points[p.index,]
p.ts <- ts[p.index,]

plot(s.points)
points(df.weapons, col='grey')
points(p.points, pch=16)

# dd1
edist <- function(a,b){sqrt(sum((a-b) ^ 2))}
n <-length(p.points@coords[,1]) 
x <-1:n 
df.d <- data.frame(p1=rep(1,n),p2=x,d1=x,d2=x)

for (i  in seq_along(p.ts[,1])) {
    df.d[i,3] <- edist(p.points@coords[1,],p.points@coords[i,])
}

for (i  in seq_along(p.ts[,1])) {
    df.d[i,4] <- edist(p.ts[1,],p.ts[i,])
}

pdf("5pictures/dd1.pdf", height=4, width=6, bg = "white") 
    plot(df.d[,3:4], pch=16, xlab="spatial distance (m)", ylab= "density distance", main="dd1")
    lines(df.d[,3:4])
    dev.off() 

#dd3
p.points <- s.points
p.ts <- ts

n <-length(p.points@coords[,1]) 
x <-1:n 
df.d <- data.frame(p1=rep(1,n),p2=x,d1=x,d2=x)

for (i  in seq_along(p.ts[,1])) {
    df.d[i,3] <- edist(p.points@coords[50,],p.points@coords[i,])
}

for (i  in seq_along(p.ts[,1])) {
    df.d[i,4] <- edist(p.ts[50,],p.ts[i,])
}

df.dd <- data.frame(id=seq(0,250000,20000)/20000+1,d1=seq(0,250000,20000),d2=seq(0,250000,20000))

for (i  in df.dd[,1]) {
    df.dd[i,3] <- mean(df.d[(df.d[,3] > df.dd[i,2] & df.d[,3] < df.dd[i+1,2]),4])
    }

pdf("5pictures/dd3.pdf", height=4, width=6, bg = "white") 
    plot(df.dd[,2:3], pch=16, xlab="spatial distance (m)", ylab= "density distance", main="dd3")
    lines(df.dd[,2:3])
dev.off() 

    
    


##################################################### 
#dd9
p.points <- s.points
p.ts <- ts

n <-length(p.points@coords[,1]) 
x <-1:n 

df.d <- data.frame(p1=rep(1,n),p2=x,d1=x,d2=x)
for (i  in 2:n) {
    df.d <- rbind(df.d,data.frame(p1=rep(i,n),p2=x,d1=x,d2=x))
}

for (i  in seq_along(df.d[,1])) {
    df.d[i,3] <- edist(p.points@coords[df.d[i,1],],p.points@coords[df.d[i,2],])
}

for (i  in seq_along(df.d[,1])) {
    df.d[i,4] <- edist(p.ts[df.d[i,1],],p.ts[df.d[i,2],])
}

df.dd <- data.frame(id=seq(0,250000,20000)/20000+1,d1=seq(0,250000,20000),d2=seq(0,250000,20000))

for (i  in df.dd[,1]) {
    df.dd[i,3] <- mean(df.d[(df.d[,3] > df.dd[i,2] & df.d[,3] < df.dd[i+1,2]),4])
}

pdf("5pictures/dd9.pdf", height=4, width=6, bg = "white") 
plot(df.dd[,2:3], pch=16, xlab="spatial distance (m)", ylab= "density distance", main="dd9")
lines(df.dd[,2:3])
dev.off() 






##################################################### 
#ende




# dd2
dist <- seq(from=0, to=33000, by=1000)
cdist2 <- cdist <- id <- 1:length(dist)
dresult <- cbind(id, dist, cdist[] <- 0, cdist2[] <- 0)
samppt   <- spsample(sgdf_srtm, 500,  type="regular")
i_kde <- extract(raster(sgdf_meg_dens), samppt)
ref_kde <- extract(raster(sgdf_meg_dens), cbind(3571203,6036796))
dmat <- matrix(1:length(dist)*length(sgdf_meg_dens@data$v), nrow=length(dist), ncol=length(sgdf_meg_dens@data$v))
dmat[] <- as.double(dmat[] <- NA)
mmean <- function (x) {mean(x, na.rm=TRUE)}
edist <-function(x1,x2,y1,y2) {sqrt((x1 - x2)^2 + (y1 - y2)^2)}

for (i in seq_along(samppt@coords[,1])) {
    x1 <- 3571203; y1 <- 6036796
    x2 <- samppt@coords[i,1]; y2 <- samppt@coords[i,2]
    if (x1 > x2) {
        sdist <-edist(x1,x2,y1,y2)
        dind <- floor(sdist/1000) + 1
        dmat[dind,i] <- abs(i_kde[i] - ref_kde)
    }
}
dresult[,3] <-  apply(dmat, 1, mmean)

for (i in seq_along(samppt@coords[,1])) {
    x1 <- 3571203; y1 <- 6036796
    x2 <- samppt@coords[i,1]; y2 <- samppt@coords[i,2]
    if (x1 < x2) {
        sdist <-edist(x1,x2,y1,y2)
        dind <- floor(sdist/1000) + 1
        dmat[dind,i] <- abs(i_kde[i] - ref_kde)
    }
}
dresult[,4] <-  apply(dmat, 1, mmean)

pdf("6pictures/c10_dd2.pdf", height=4, width=6, bg = "white") 
plot(x=dresult[,2], y=dresult[,4], col="grey", pch=16, xlab="spatial distance (m)", ylab= "density distance")
lines(x=dresult[,2], y=dresult[,4],lty=1,col="grey")
points(x=dresult[,2], y=dresult[,3], pch=16, col="black")
lines(x=dresult[,2], y=dresult[,3],lty=1,col="black")
dev.off() 

# dd7
dist <- seq(from=0, to=33000, by=1000)
cdist <- id <- 1:length(dist)
dresult <- cbind(id, dist, cdist[] <- 0)
dresulta <- dresult
samppt   <- spsample(sgdf_srtm, 500,  type="regular")
i_kde <- extract(raster(sgdf_meg_dens), samppt)
#ref_kde <- extract(raster(sgdf_meg_dens), cbind(3571203,6036796))
dmat <- matrix(1:length(dist)*length(sgdf_meg_dens@data$v), nrow=length(dist), ncol=length(sgdf_meg_dens@data$v))
dmat[] <- as.double(dmat[] <- NA)
mmean <- function (x) {mean(x, na.rm=TRUE)}
edist <-function(x1,x2,y1,y2) {sqrt((x1 - x2)^2 + (y1 - y2)^2)}

for (j in seq_along(samppt@coords[,1])) {
    for (i in seq_along(samppt@coords[,1])) {
        x1 <- samppt@coords[j,1]; y1 <- samppt@coords[j,2]
        x2 <- samppt@coords[i,1]; y2 <- samppt@coords[i,2]
        if (x1 == x2 & y2 >= y1) {
            sdist <-edist(x1,x2,y1,y2)
            dind <- floor(sdist/1000) + 1
            dmat[dind,i] <- abs(i_kde[i] - i_kde[j])
        }
    }
    dresult[,3] <-  apply(dmat, 1, mmean)
    dresulta[,3] <- dresulta[,3] + dresult[,3]
    dresult <- cbind(id, dist, cdist[] <- 0)
}

pdf("6pictures/c10_dd7.pdf", height=4, width=6, bg = "white") 
plot(x=dresulta[,2], y=dresulta[,3], col="black", pch=16, xlab="spatial distance (m)", ylab= "density distance")
lines(x=dresulta[,2], y=dresulta[,3],lty=1,col="black")
dev.off() 

# dd9
dist <- seq(from=0, to=33000, by=1000)
cdist <- id <- 1:length(dist)
dresult <- cbind(id, dist, cdist[] <- 0)
dresulta <- dresult
samppt   <- spsample(sgdf_srtm, 500,  type="regular")
i_kde <- extract(raster(sgdf_meg_dens), samppt)
#ref_kde <- extract(raster(sgdf_meg_dens), cbind(3571203,6036796))
dmat <- matrix(1:length(dist)*length(sgdf_meg_dens@data$v), nrow=length(dist), ncol=length(sgdf_meg_dens@data$v))
dmat[] <- as.double(dmat[] <- NA)
mmean <- function (x) {mean(x, na.rm=TRUE)}
edist <-function(x1,x2,y1,y2) {sqrt((x1 - x2)^2 + (y1 - y2)^2)}

for (j in seq_along(samppt@coords[,1])) {
    for (i in seq_along(samppt@coords[,1])) {
        x1 <- samppt@coords[j,1]; y1 <- samppt@coords[j,2]
        x2 <- samppt@coords[i,1]; y2 <- samppt@coords[i,2]
        sdist <-edist(x1,x2,y1,y2)
        dind <- floor(sdist/1000) + 1
        dmat[dind,i] <- abs(i_kde[i] - i_kde[j])
    }
    dresult[,3] <-  apply(dmat, 1, mmean)
    dresulta[,3] <- dresulta[,3] + dresult[,3]
    dresult <- cbind(id, dist, cdist[] <- 0)
}

pdf("6pictures/c10_dd9.pdf", height=4, width=6, bg = "white") 
plot(x=dresulta[,2], y=dresulta[,3], col="black", pch=16, xlab="spatial distance (m)", ylab= "density distance")
lines(x=dresulta[,2], y=dresulta[,3],lty=1,col="black")
dev.off() 



