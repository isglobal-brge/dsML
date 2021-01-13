#' @title Create dummy variables
#'
#' @param x \code{data.frame} Data frame to which create dummies for categorical variables
#'
#' @return \code{data.frame} Data frame with categorical variables transformed to dummies
#' @export

dummiesDS <- function(x){
  data <- mlr::createDummyFeatures(x)
  class(data) <- c(class(data), "dummies")
  return(data)
}