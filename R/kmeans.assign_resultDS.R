#' @title Assign values as a column on a data frame
#' 
#' @description This function is called from the ds.kmeans function when \code{assign = TRUE},
#' it could be adapted to other functionalities checking if lengths are equal.
#' 
#' @param x \code{data frame} Table where the next arguments will be added as a column
#' @param ... \code{numeric} Values to be coerced as a column to the table 'x'
#'
#' @return \code{data frame} With the original 'x' table with a column added named 'kmeans.cluster'.
#' The added column is encoded as a factor and as such passes through the disclosive check: checking if the
#' number of levels of the factor is more than nfilter.levels (percentage) of the length of the variable.
#' @export

kmeans.assign_resultDS <- function(x, ...){
  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS                    #
  thr <- listDisclosureSettingsDS()                         #
  #nfilter.tab <- as.numeric(thr$nfilter.tab)                 #
  #nfilter.glm <- as.numeric(thr$nfilter.glm)                 #
  #nfilter.subset <- as.numeric(thr$nfilter.subset)           #
  #nfilter.string <- as.numeric(thr$nfilter.string)           #
  #nfilter.stringShort <- as.numeric(thr$nfilter.stringShort) #
  #nfilter.kNN <- as.numeric(thr$nfilter.kNN)                  #
  #nfilter.noise <- as.numeric(thr$nfilter.noise)              #
  nfilter.levels <- as.numeric(thr$nfilter.levels)           #
  #############################################################
  
  dots <- unlist(list(...))
  nfilter.levels <- ceiling(length(dots) * nfilter.levels)
  dots <- as.factor(dots)

  if(length(levels(dots)) > nfilter.levels){
    stop(paste0("Maximum factor level [", nfilter.levels, "] surpased with [", length(levels(dots)), "] levels"))
  }
  return(cbind(x, kmeans.cluster = dots))
}