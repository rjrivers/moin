# DETERRENCE FUNCTIONS ---------------------------------------------------------

minimum_distance <- function(mat, min) {
  mat[mat < min] <- NA

  # Warn if some nodes are "stranded" from the rest of the graph
  if(rowSums(is.na(mat)) == ncol(mat) ||
     colSums(is.na(mat)) == nrow(mat) ) {
    warning("Some nodes are not connected to the graph. Check min value.")
  }

  return(mat)
}
