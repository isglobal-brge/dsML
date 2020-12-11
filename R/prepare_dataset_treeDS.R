#' Title
#'
#' @param object 
#' @param questions 
#'
#' @return
#' @export
#'
#' @examples
prepare_dataset_treeDS <- function(object, questions){
  
  questions <- unserialize(wkb::hex2raw(questions))
  
  for(i in 1:nrow(questions)){
    if(questions[i,4] == "numerical"){
      value <- as.numeric(substr(questions[i,3], 7, nchar(questions[i, 3])))
      if(substr(questions[i,3], 1, 5) == "lower"){
        object <- object[object[[questions[i,2]]] <= value, ]
      }
      else{
        object <- object[object[[questions[i,2]]] > value, ]
      }
    }
    else{
      value <- substr(questions[i,3], 7, nchar(questions[i, 3]))
      if(substr(questions[i,3], 1, 5) == "equal"){
        object <- object[object[[questions[i,2]]] == value, ]
      }
      else{
        object <- object[object[[questions[i,2]]] != value, ]
      }
    }
  }
  return(object)
}