#' @title Transform table
#' 
#' @description Divide columns of table by a vector of values,
#' this function is to be used to transform tables that only contain dummies for the FAMD functionality.
#' Other uses may yield to errors as there is no checks for coherence between the table and the
#' vector.
#'
#' @param x  \code{data.frame}
#' @param ... \code{numerical} Vector to divide the table
#'
#' @return \code{data.table}
#' @export

dummies_transformationDS <- function(x, ...){
  
  values <- unlist(list(...))
  return(x / sqrt(values))
  
}