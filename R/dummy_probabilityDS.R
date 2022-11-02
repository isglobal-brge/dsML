#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
dummy_probabilityDS <- function(x){
  
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.subset <- as.numeric(thr$nfilter.subset)
  
  if(nrow(x) < nfilter.subset){stop("Subset is too small")}
  sapply(x, function(w) sum(w == 1))
  
}