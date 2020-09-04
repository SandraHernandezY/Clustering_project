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
  
  #distance[1]:id centroide distance[2]: distancia minima
  distance <-list()
  
  for (i in 1:length(sismos)){
    distance[1] <-1
    d <-distHaversine(c(sismos[[i]][[2]],sismos[[i]][[3]]),c(centroids[[1]][[2]],centroids[[1]][[3]]),r= 6371.0)
    distance[2] <-d
    for (j in 1:length(centroids)) {
      aux <- distHaversine(c(sismos[[i]][[2]],sismos[[i]][[3]]),c(centroids[[j]][[2]],centroids[[j]][[3]]),r= 6371.0)
      if(aux < distance[2]){
        distance[1] <- j
        distance[2] <- aux
      }
    }
    #Asignar centroide mas cercano al sismo i
    sismos[[i]][[9]]= as.numeric(distance[1])
  }
  
  # Recalculando centroides
 
  latitude= c()
  longitude= c()
  
  #Arreglo de pruebas
  
  for (j in 1:length(centroids)){
    latitude<-c()
    longitude<- c()
    
    for (i in 1:length(sismos)){
      if(sismos[[i]][[9]] == j){
        latitude = c(latitude,sismos[[i]][[2]])
        longitude= c(longitude,sismos[[i]][[3]])
      }
    }
    #-----prueba-------
    #c("centroide ",j)
    #print("latitude")
    #print(latitude)
    #print("longitude")
    #print(longitude)
    #--------------------
    
    # calculando mediana
    new_latitude <- median(latitude)
    new_longitude <- median(longitude)
    centroids[[j]][[2]] = new_latitude
    centroids[[j]][[3]] = new_longitude
    
  }
  
  return  <- list(sismos,centroids)
  return(return)
}

#------------------ PRUEBAS ---------------------------
#' Inicializando soluciones inicial           
#centroids <- initialSolution(sismos, num_centroids=3)

# prubas kmedians <- function(sismos, centroids, num_clusters)
#sismos <- kmedians(sismos, centroids)

#for (i in 1:length(sismos)) {
#  print(resul[[i]][[9]])
#}
#
#
#
#