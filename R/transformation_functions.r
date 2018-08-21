

#' spatgraph_transform
#'
#' @param x a spatgraph object 
#' @param output a character defining the output as an igraph object ("igraph"), a tidygraph object ("tbl_graph") or a neighbourhood matrix ("matrix")
#' @param mode Character scalar, it specifies whether the graph to create is undirected (‘all’ or ‘total’) or directed; and in the latter case, whether it contains the outgoing (‘out’) or the incoming (‘in’) neighbors of the vertices.
#' @param ... Further parameters given internal to the \code{graph_from_adj_list} function of the igraph package 
#'
#' @return depends on the argument output 
#' @export
#'
#' @examples 
#' x <- matrix(runif(50*2), ncol=2)
#' g <- spatgraphs::spatgraph(x, "knn", par=3)
#' #moin::spatgraph_transform(x = g, output = "tidygraph")
#' # The function can also produce a directed graph 
#' moin::spatgraph_transform(x = g, output = "tidygraph", mode = "in")
#' 
spatgraph_transform <- function(x,output="igraph",mode="all",...){
  igraphobj <- igraph::graph_from_adj_list(x$edges, mode=mode, ...)
  
  nbmatrix <- igraph::as_adjacency_matrix(igraphobj,sparse=FALSE)
  
  tidygraphobj <- tidygraph::as_tbl_graph(igraphobj)#,directed=FALSE)
  
  return(switch(output, "igraph" = igraphobj, 
                "matrix" = nbmatrix, 
                "tidygraph" = tidygraphobj))
}
