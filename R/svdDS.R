#' @title Perform block operation SVD
#' 
#' @details http://dx.doi.org/10.1137/16M1058467 For information on the SVD block method
#'
#' @param x \code{data frame} Data frame to perform the SVD
#'
#' @return \code{data frame} with the block SVD (left singluar vectors * singular values)
#' @export

svdDS <- function(x){
  
  x <- t(x)
  ss <- svd(x)
  ans <- sweep(ss$u, 2, FUN="*", ss$d)
  
}
# AGGREGATE FUNCTION
# svdDS