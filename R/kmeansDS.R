#' @title Parallel k-means iteration
#' 
#' @description Performs an iteration of a k-means parallel algorithm (what in a multi-thread machine 
#' would be run on each thread). The client acts as the master and the servers as the slaves if thinking
#' like a regular parallel implementation.
#'
#' @param x \code{data frame} Train dataset for the k-means
#' @param ... \code{numeric} Parameters corresponding to the data frame from the server that contains the
#' centroids (updated on each iteration on the client)
#'
#' @return \code{list} with: \cr
#' -counts \code{numeric} vector with the counts per cluster \cr
#' -centers \code{data frame} New centroids calculated \cr
#' -assignations \code{numeric vector} ordered cluster assignations, to be used by the client
#' to assign them with the \code{kmeans.assign_result} function to a table on the servers to be later used
#' @export

kmeansDS <- function(x, ...){
  
  # Check 'x' for NAs, this algorithm does not work with NAs in the dataset
  if(any(is.na(x))){
    stop(paste0("There are missings on the data frame ['", x, "'], ds.kmeans does not accept a data frame with missings."))
  }
  
  # Read dots and coerce to a matrix, the number of columns of this matrix has to be the same as the
  # data frame 'x'. This dots matrix corresponds to the centroids passed by the client, which will be used
  # to clusterize the dataset and will be updated at the end of this function before being returned to the client
  dots <- unlist(list(...))
  nvar <- ncol(x)
  centroids <- t(matrix(dots, nrow = nvar))
  
  # Get distances matrix, which contains the distances between the centroids and each datapoint on 'x'
  distances <- matrix(0, nrow = nrow(centroids), ncol = nrow(x))
  for(i in 1:nrow(centroids)){
    distances[i,] <- t(as.matrix(dist(rbind(centroids[i,], x)))[-1,1])
  }
    # Get index of minimums (index corresponds to which centroid is closer to each point)
  assignations <- apply(distances, 2, which.min)
    # Merge cluster results to 'x'
  x <- data.table::data.table(cbind(x, assignations))
  
  # Get the new centroids by getting the mean of each variable for each cluster. Calculate also the
  # number of points on each cluster
  new_means <- aggregate(x, list(x$assignations), mean)
  cluster_centers <- new_means[,1:(nvar+1)]
  cluster_numbers <- aggregate(x, list(x$assignations), length)[,1:2]
  colnames(cluster_numbers) <- c("assignations","count")
    # Sometimes if one of the centroids passed to this function is too far, the points get classified
    # only on the other centroids, the following if clauses serve to check for that. When a centroid did
    # not get any points assigned, the input centroid will be returned to the client with an assigned number
    # of points 0.
  if(nrow(cluster_numbers) < nrow(centroids)){
    missing_centers <- as.numeric(1:nrow(centroids))[!(1:nrow(centroids) %in% cluster_numbers$assignations)]
    for(i in missing_centers){
      cluster_numbers <- rbind(cluster_numbers, c(i, 0))
    }
  }
  data.table::setorder(cluster_numbers, assignations)
  if(nrow(cluster_centers) < nrow(centroids)){
    missing_centers <- as.numeric(1:nrow(centroids))[!(1:nrow(centroids) %in% cluster_centers$Group.1)]
    for(i in missing_centers){
      cluster_centers <- rbind(cluster_centers, c(i, t(centroids[i,])))
    }
  }
  data.table::setorder(cluster_centers, Group.1)
  
  # Return new centroids, assigned points to each centroid and the assignations vector
  return(list(counts = cluster_numbers, centers = cluster_centers[,-1], assignations = x$assignations))
  
}