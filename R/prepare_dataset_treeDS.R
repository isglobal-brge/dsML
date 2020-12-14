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
  
  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS                    #
  thr <- dsBase:::listDisclosureSettingsDS()                           #
  #nfilter.tab <- as.numeric(thr$nfilter.tab)                 #
  #nfilter.glm <- as.numeric(thr$nfilter.glm)                 #
  nfilter.subset <- as.numeric(thr$nfilter.subset)           #
  #nfilter.string <- as.numeric(thr$nfilter.string)           #
  #nfilter.stringShort <- as.numeric(thr$nfilter.stringShort) #
  # nfilter.kNN <- as.numeric(thr$nfilter.kNN)                  #
  # nfilter.noise <- as.numeric(thr$nfilter.noise)              #
  #nfilter.levels <- as.numeric(thr$nfilter.levels)           #
  #############################################################
  
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
    object <- object[, !(names(object) %in% questions[i,2])]
  }
  
  subset.size <- nrow(object)
  
  if(subset.size < nfilter.subset){
    studysideMessage<-"Subset to be created is too small (<nfilter.subset)"
    stop(list(studysideMessage=studysideMessage))
  }
  
  return(object)
}