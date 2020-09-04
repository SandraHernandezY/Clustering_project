#set.seed(123)
# Lectura de datos
library(geosphere) 
tusDatos <- read.table(file.choose(), skip = 0, header = TRUE, sep =',')

sismos <- list()

# Creando y llenando la estructura de datos listas de listas `sismos`
for(i in 1:nrow(tusDatos)) {
  sismos[[i]] <- list(tusDatos[i,1],tusDatos[i,2],tusDatos[i,3],tusDatos[i,4],tusDatos[i,5],tusDatos[i,6],tusDatos[i,7],tusDatos[i,8],tusDatos[i,9])
}

# initialSolution: inicializacion de centroides 

initialSolution <- function(sismos, num_centroids){
  
  centroid <- list()
  x <- sample(1:length(sismos), num_centroids, replace=F)
 
  for(i in 1:num_centroids) {
    #print(x[i])
    centroid[[i]] <- list(i,sismos[[x[i]]][[2]],sismos[[x[i]]][[3]])
  }
  return (centroid)
  
}

#' kmedians
#'
#' Groups the points in your dataset ,X, into the desired number of clusters, based on the median distance between the points.
#' This function uses random intilization to assign the first medians and then will update the medians and
#' the group assignments until the assignment does not change.
#'
#' @param sismos            lista de listas, conjunto de datos
#' @param num_clusters      integer, numero de clusters deseados
#'
#' @return sismos           lista de lista, datos asociados a su centroide mas cercanos 
#'          


kmedians <- function(sismos, centroids){
  #print(centroids)
  
  distance <-list()      #[1]:id centroide [2]: distancia minima
  distance[1] <-1
  d <-distHaversine(c(sismos[[1]][[2]],sismos[[1]][[3]]),c(centroids[[1]][[2]],centroids[[1]][[3]]),r= 6371.0)
  distance[2] <- d
  
  for (i in 1:length(sismos)){
    for (j in 1:length(centroids)) {
      aux <- distHaversine(c(sismos[[i]][[2]],sismos[[i]][[3]]),c(centroids[[j]][[2]],centroids[[j]][[3]]),r= 6371.0)
      if(aux < distance[2]){
        distance[1] <- j
        distance[2] <- aux
        print("entre")
      }
    }
    #Asignar centroide mas cercano al sismo i,    no es necesario guardar la distancia encontras?
    sismos[[i]][[9]]= as.numeric(distance[1])
  }
  return(sismos)
}

#------------------ PRUEBAS ---------------------------
#' Inicializando soluciones inicial           
centroids <- initialSolution(sismos, num_centroids=5)

# prubas kmedians <- function(sismos, centroids, num_clusters)
resul <- kmedians(sismos, centroids)

for (i in 1:length(sismos)) {
  print(resul[[i]][[9]])
}
