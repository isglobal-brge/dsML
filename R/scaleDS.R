#' @title Scaling of a data frame or numeric vector
#' 
#' @description Scales each column of a data frame object by dividing for the standard deviation of the column
#'
#' @param table \code{data.frame} or \code{numeric} vector to be scaled
#' @param ... \code{numeric} The standard deviations of the columns (ordered by column)
#'
#' @return \code{data.frame} or \code{numeric} vector scaled
#' @export

scaleDS <- function(table, ...){
  
  dots <- unlist(list(...))
  
  if(class(table)  == "numeric"){
    # Integrity check
    if(length(dots) != 1){
      stop("Too many dot arguments passed, only one needed")
    }
    try(table <- table / dots)
  }
  else{
    if(length(dots) != ncol(table)){
      stop(paste0("Number of standard deviation values passed [", length(dots), "], does not match
                the number of columns of the table [", ncol(table), "]"))
    }
    
    for(i in 1:ncol(table)){
      try(table[, i] <- table[, i] / dots[i])
    }
  }
  
  return(table)
  
}
# ASSIGN FUNCTION
# scaleDS