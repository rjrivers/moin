# kde nur für die Knoten

## Testdaten
x1 <- c(3,5,3,2)
y1 <- c(5,2,3,6)
x2 <- c(5,6,8,2,2,4,5,4,7,5,6)
y2 <- c(6,9,9,6,7,4,8,5,2,1,1)


## Funktionen


# Hilfsfunktion für die Benutzung in apply wird die dist() gewrapt
dist4 <- function(x){
  x_1 <- x[1]
  x_2 <- x[2]
  y_1 <- x[3]
  y_2 <- x[4]
  m <- matrix(c(x_1,x_2,y_1,y_2), nrow=2)
  d <- dist(m)[1]
  return(d)
}


# es werden die Koordinaten der Funde eines Typs übergeben (x2,y2) und für alle Knoten (x1,y1) die Dichtewerte als Vektor ausgegeben
dens_samp <- function(x1,y1,x2,y2,sd) {
    l <- length(x2)
    ts <- x1
    ts[] <- 0

    for (i in 1:length(x1)) {
      xav <- rep(x1[i], l)
      yav <- rep(y1[i], l)
      xy <- cbind(xav,yav,x2,y2)
      d <- apply(xy, 1, dist4)
      d <- dnorm(d, mean=0, sd=sd)
      ts[i] <- sum(d)
    }
    return(ts)
}


# Funktion die für jeden Typ dens_samp aufruft
# ...

