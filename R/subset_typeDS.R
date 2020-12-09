#' @title Subset data frame by class
#' 
#' @description Create a subset of a data frame with the columns that have a shared class
#'
#' @param table \code{data frame} Table to be subsetted
#' @param type \code{character} Class to which be subsetted. If \code{numeric} only the \code{numeric} columns
#' will be on the output table
#'
#' @return \code{data frame} Subsetted table
#' @export

subset_typeDS <- function(table, type){
  
  n_cols <- ncol(table)
  
  cols_to_keep <- NULL

  for(col in 1:n_cols){
    # print(classDS(paste0(data, "$", col)))
    if(class(table[, col]) == type){
      cols_to_keep <- c(cols_to_keep, col)
    }
  }
  
  new_table <- subset(x = table, select = cols_to_keep)
  
  return(new_table)
  
}