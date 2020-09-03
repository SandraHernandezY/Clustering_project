#' distance
#'
#' Calculates the Manhanttan distance between the medians and every point in the dataset
#'
#' @param X a matrix, the dataset being clustered
#' @param medians a matrix, dedians of the clusters
#'
#' @return a matrix, distance between each point and each median
#' @export
#'
#' @examples
#' A <- matrix(
#' c(1,2,3,4,5,6),
#' nrow = 3,
#' ncol = 2,
#' byrow = TRUE)
#' m <- matrix(
#'   c(1,1,2,2),
#'   nrow = 2,
#'   ncol = 2,
#'  byrow = TRUE)
#' distance(A,m)
#'

distance <- function(X, medians){
  
  # Check that inputs are valid and as expected
  if(!is.matrix(X)) stop("Input X should be a matrix!")
  
  if(!is.matrix(medians)) stop("Input medians should be a matrix!")
  
  if(nrow(X)  < (nrow(medians))) stop("non-numeric matrix extent")
  
  K = nrow(medians)
  n = nrow(X)
  
  dist<- matrix(nrow=n,ncol=K)
  
  for (k in 1:K) {
    for (i in 1:n){
      dist[i,k] <- abs(X[i,1]-medians[k,1])+abs(X[i,2]-medians[k,2])
    }
  }
  
  return (dist)
}


#' kmedians
#'
#' Groups the points in your dataset ,X, into the desired number of clusters, based on the median distance between the points.
#' This function uses random intilization to assign the first medians and then will update the medians and
#' the group assignments until the assignment does not change.
#'
#' @param X a matrix, the dataset being clustered
#' @param num_clusters integer, the desired number of clusters
#' @param n_it integer, number of iterations
#'
#' @return list, contains both medians and labes
#'         medians: matrix
#'         The coordinates of the medians for each cluster
#'
#'         labels: list
#'          List that has the assignment of the cluster for each point in the dataset

kmedians <- function(X, num_clusters,n_it=100){
  
  # Check that inputs are valid and as expected
  if(!is.matrix(X)) stop ("Input X should be a matrix!")
  
  if(round(num_clusters)!=num_clusters) stop ("Input number of clusters should be an integer!")
  
  if(is.character(num_clusters)) stop ("non-numeric argument to mathematical function")
  
  if(is.null(num_clusters)) stop ("non-numeric argument to mathematical function")
  
  if(round(n_it)!=n_it) stop ("Input number of iterations should be an integer!")
  
  if((nrow(X)  <= num_clusters)) stop ("cannot take a sample larger than the population when 'replace = FALSE'")
  
  
  set.seed(123)
  n <- nrow(X)
  u <- matrix(0, nrow = num_clusters, ncol = n)
  
  # initialize median points
  medians <- X[sample(n,size=num_clusters,replace=FALSE),]
  
  for (i in 1:n_it){
    
    K <- nrow(medians)
    N <- nrow(X)
    
    old_medians <- medians
    
    dist <- distance(X, medians)
    
    labels <- apply(dist, 1, which.min)
    
    for (j in 1:n){
      u[labels[j], j] <- 1
    }
    
    for (k in 1:num_clusters){
      medians[k,] <- apply((matrix(X[u[k,]==1],ncol=2)), 2, median)
    }
    
    if (identical(medians,old_medians)){
      break
    }
  }
  # make the output as a list
  return(list(medians,labels))
}