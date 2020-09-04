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
#' 
#' m <- matrix(
#'   c(1,1,2,2),
#'   nrow = 2,
#'   ncol = 2,
#'  byrow = TRUE)
#'  
#' distance(A,m)
#'

distance <- function(X, medians){
  
  # Check that inputs are valid and as expected
  if(!is.matrix(X)) stop("Input X should be a matrix!")
  
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
# Lectura de datos
tusDatos <- read.table(file.choose(), skip = 0, header = TRUE, sep =',')
# Initialize `sismos`
sismos <- list()
# Creando y llenando la estructura de datos listas de listas `sismos`
for(i in 1:nrow(tusDatos)) {
  sismos[[i]] <- list(tusDatos[i,1],tusDatos[i,2],tusDatos[i,3],tusDatos[i,4],tusDatos[i,5],tusDatos[i,6],tusDatos[i,7],tusDatos[i,8],tusDatos[i,9])
}

# initialSolution: inicializacion de centroides 
# se busca un x aleatorio para luego extraer latitud y longitud del sismo x e inicializar lista de centroides
initialSolution <- function(sismos, num_centroids){
  
  centroid <- list()
  x <- sample(1:length(sismos), num_centroids, replace=F)
 
  for(i in 1:num_centroids) {
    #x <- sample(1:length(sismos), 1) 
    print(x[i])
    centroid[[i]] <- list(i,sismos[[x[i]]][[2]],sismos[[x[i]]][[3]])
  }
  return (centroid)
}
#PRUEBA
centrides <- initialSolution(sismos, num_centroids=3)
  

#' kmedians
#'
#' Groups the points in your dataset ,X, into the desired number of clusters, based on the median distance between the points.
#' This function uses random intilization to assign the first medians and then will update the medians and
#' the group assignments until the assignment does not change.
#'
#' @param sismos            lista de listas, conjunto de datos
#' @param num_clusters      integer, numero de clusters deseados
#' @param n_it              integer, number of iterations
#'
#' @return list,            contiene medianas y etiquetas
#'         medians: matrix  Las coordenadas de las medianas de cada grupo.
#'
#'         labels: list     Lista que tiene la asignación del clúster para cada punto del conjunto de datos
#'          
kmedians <- function(sismos, centroids, num_clusters,n_it=100){
  
  set.seed(123)
  n <- length(sismos)       #numero de sismos(filas)
  u <- matrix(0, nrow = num_clusters, ncol = n) #matriz de n fila= cant.cluster, c/column tendra los sismos que fueron asignado a dicho cluster 
 
  centroides <- initialSolution(sismos, num_centroids)
    
  medians <- sismos[sample(n,size=num_clusters,replace=FALSE),] #inicializando puntos medianos
  
 
  for (i in 1:n_it){
    
    K <- nrow(medians)
    N <- length(sismos) #nrow(sismos)
    
    old_medians <- medians
    
    dist <- distance(sismos, medians)
    
    labels <- apply(dist, 1, which.min)
    
    for (j in 1:n){
      u[labels[j], j] <- 1
    }
    
    for (k in 1:num_clusters){
      medians[k,] <- apply((matrix(sismox[u[k,]==1],ncol=2)), 2, median)
    }
    
    if (identical(medians,old_medians)){
      break
    }
  }
  # make the output as a list
  return(list(medians,labels))
}
