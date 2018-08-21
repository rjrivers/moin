# DETERRENCE FUNCTIONS ---------------------------------------------------------

mdm <- function(mat, min) {
  mat %>%
    is_less_than(min) %>%
    as.integer() ->
    mat

  # Warn if some nodes are "stranded" from the rest of the graph
  if(rowSums(mat) == 0 ||
     colSums(mat) == 0) {
    warning("Some nodes are not connected to the graph. Check min value.")
  }

  return(mat)
}
