#' @title K-Nearest Neighbour Classification
#' 
#' @description Compute K-Nearest Neighbours of a query vector
#'
#' @param x \code{data frame} Dataset to get the neighbours and tags
#' @param neigh \code{numeric} number of neighbours considered
#' @param classificator_name \code{character} Name of column on the table 'x' that has the classifier factor
#' @param method.indicator \code{character} (default \code{"knn"}) specifies the method that is used to
#' generated non-disclosive coordinates to calculate the euclidean distance. This argument can be set as \code{'knn'}
#'  or \code{'noise'}
#' @param k \code{numeric} (default \code{3}) he number of the nearest neighbors for which their centroid is calculated
#' @param noise \code{numeric} (default \code{0.25}) the percentage of the initial variance that is used as the variance 
#' of the embedded noise if the argument method is set to \code{'noise'}
#' @param ... \code{numeric} Queried vector
#'
#' @return \code{list} with: \cr
#' -distance \code{numeric}: Distances of the queried vector to the anonimized dataset \cr
#' -classification \code{character}: Clasification tag of the queried vector
#' @export

knnDS <- function(x, neigh, classificator_name, method.indicator, k, noise, ...){
  
  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS                    #
  thr <- listDisclosureSettingsDS()                           #
  #nfilter.tab <- as.numeric(thr$nfilter.tab)                 #
  #nfilter.glm <- as.numeric(thr$nfilter.glm)                 #
  #nfilter.subset <- as.numeric(thr$nfilter.subset)           #
  #nfilter.string <- as.numeric(thr$nfilter.string)           #
  #nfilter.stringShort <- as.numeric(thr$nfilter.stringShort) #
  nfilter.kNN <- as.numeric(thr$nfilter.kNN)                  #
  nfilter.noise <- as.numeric(thr$nfilter.noise)              #
  nfilter.levels <- as.numeric(thr$nfilter.levels)            #
  #############################################################
  
  # Check 'x' for NAs, this algorithm does not work with NAs in the dataset
  if(any(is.na(x))){
    stop(paste0("There are missings on the data frame ['", x, "'], ds.knn does not accept a data frame with missings."))
  }
  
  # Separate 'x' into 'classificator' (column of 'x' with the factor classification variable) and overwrite 'x'
  # with the original data frame minus the 'classificator' column
  classificator <- x[, names(x) %in% classificator_name]
  x <- x[, !(names(x) %in% classificator_name)]
  
  # Disclosive check on the classificator column, as it will be forced into a factor and it will be passed
  # to the client
  nfilter.levels <- ceiling(length(classificator) * nfilter.levels)
  classificator <- as.factor(classificator)
  
  if(length(levels(classificator)) > nfilter.levels){
    stop(paste0("Maximum factor level [", nfilter.levels, "] surpased with [", length(levels(classificator)), "] levels"))
  }
  
  # Get number of rows of 'x' for various uses inside this function
  N.data <- dim(x)[1]
  
  # Unlist the dots and merge into a vector, this is the vector that will be queried for distances against all the datapoints
  # on 'x'
  query <- unlist(list(...))
  
  # Anonimize 'x' using knn centroids or noise, adapted from the scatterPlotDS function. This is done because the distances
  # will be passed to the client, so if the client queries an exact point the distance returned will be 0 which would disclose
  # the information of a point and the server data could be reconstructed on the client
  if(method.indicator == "knn"){
    if(k < nfilter.kNN | k > (N.data - nfilter.kNN)){
      stop(paste0("k must be greater than or equal to ", nfilter.kNN, "and less than or equal to ", (N.data-nfilter.kNN), "."), call.=FALSE)
    }else{
      neighbours = k
    }
    nearest <- RANN::nn2(x, k = neighbours)
    x.non_disclosive <- matrix(0, nrow = N.data, ncol = ncol(x))
    for(i in 1:N.data){
      for(j in 1:ncol(x)){
        x.non_disclosive[i,j] <- mean(x[nearest$nn.idx[i,],][,j])
      }
    }
    x.non_disclosive <- data.frame(x.non_disclosive)
    colnames(x.non_disclosive) <- colnames(x)
  }
  else if(method.indicator == "noise"){
    if(noise < nfilter.noise){
      stop(paste0("'noise' must be greater than or equal to ", nfilter.noise), call.=FALSE)
    }else{
      percentage <- noise
    }
    seed <- getOption("datashield.seed")
    if (is.null(seed))
      stop("scatterPlotDS requires 'datashield.seed' R option to operate", call.=FALSE)
    set.seed(seed)
    x.non_disclosive <- matrix(0, nrow = N.data, ncol = ncol(x))
    for(i in 1:ncol(x)){
      x.non_disclosive[,i] <- x[,i] + stats::rnorm(N.data, mean=0, sd=sqrt(percentage*stats::var(x[,i])))
    }
  }
  else(stop(paste0("Invalid method: ", method.indicator, ". Methods supported: 'knn' and 'noise'")))
  
  # Get euclidean distance from 'query' to all the datapoints on 'x', correct names and order by distance (ascending)
  query_distances <- as.matrix(dist(rbind(query, x.non_disclosive)))[-1,1]
  names(query_distances) <- as.character(1:N.data)
  query_distances_indexes <- order(query_distances)[1:neigh]
  
  # Return a list with the distances and classification tag (number of values = neigh)
  return(list(distance = query_distances[query_distances_indexes], 
                    classification = classificator[query_distances_indexes]))
  
}
