#' @title Centering of a data frame
#' 
#' @description Centers each column of a data frame object by substracting the mean of the column
#'
#' @param table \code{data.frame} or \code{numeric} vector to be centered
#' @param ... \code{numeric} The means of the columns (ordered by column)
#'
#' @return \code{data.frame} or \code{numeric} vector centered
#' @export

centerDS <- function(table, ...){

  dots <- unlist(list(...))
  
  if(class(table)  == "numeric"){
    # Integrity check
    if(length(dots) != 1){
      stop("Too many dot arguments passed, only one needed")
    }
    try(table <- table - dots)
  }
  else{
    if(length(dots) != ncol(table)){
      stop(paste0("Number of mean values passed [", length(dots), "], does not match
                the number of columns of the table [", ncol(table), "]"))
    }
    
    for(i in 1:ncol(table)){
      try(table[, i] <- table[, i] - dots[i])
    }
  }

  return(table)
  
}
# ASSIGN FUNCTION
# centerDS