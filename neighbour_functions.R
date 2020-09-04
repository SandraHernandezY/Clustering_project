library(geosphere)
library(seqinr)


# initialSolution: inicializacion de centroides 
# se busca un x aleatorio para luego extraer latitud y longitud del sismo x e inicializar lista de centroides
initialSolution <- function(sismos, num_centroids){
  
  centroid <- list()
  x <- sample(1:length(sismos), num_centroids, replace=F)
  
  for(i in 1:num_centroids) {
    #x <- sample(1:length(sismos), 1) 
    #print(x[i])
    centroid[[i]] <- list(i,sismos[[x[i]]][[2]],sismos[[x[i]]][[3]])
  }
  return (centroid)
}

#Función para calcular distancia promedio de cada cluster
calculateAVGdistance <- function(centroids,sismos){
  list_avg <- list()
  for (i in 1:length(centroids)) {
    distancias <- list()
    for (j in 1:length(sismos)) {
      if(sismos[[j]][[2]]!=centroids[[i]][[2]] & sismos[[j]][[3]]!=centroids[[i]][[3]]){
        if(sismos[[j]][[9]] == i){
          d <- distHaversine(c(sismos[[j]][[2]],sismos[[j]][[3]]),c(centroids[[i]][[2]],centroids[[i]][[3]]),r= 6371.0) 
          distancias <- append(distancias,d)
        }
      }
    }
    avg <- mean(as.numeric(distancias))
    list_avg <- append(list_avg,avg)
  }
  return (list_avg)
}

#Función de vecindad promedio
#Entrada: centroids (lista centroides), avg_dist (lista distancia promedio de cada cluster), datos con su cluster correspondiente
averageNeighbour <- function(centroids, avg_dist, sismos){ 
  len_centroids<-length(centroids)
  len_sismos<-length(sismos)
  for(i in 1:len_centroids){
    for(j in 1:len_sismos) {
      rand_position <- sample(1:len_sismos, 1)
      print(rand_position)
      while(sismos[[rand]][[9]] != i){
        rand_position <- sample(1:len_sismos, 1)
        print(rand_position)
      }
      rand_data <- sismos[rand_position]
      distance <- distHaversine(c(rand_data[2],rand_data[3]),c(centroids[[i]][[2]],centroids[[i]][[3]]),r= 6371.0)
      if(distance <= avg_dist[i]) {
        centroids[[i]][[2]]=rand_data[2]
        centroids[[i]][[3]]=rand_data[3]
        break
      }
    }
  }
  return (centroids)
}

#Función de random_swap
#Entrada: centroids (lista centroides), datos con su cluster correspondiente
randomSwap <- function(centroids, sismos){ 
  len_centroids<-length(centroids)
  len_sismos<-length(sismos)
  
  rand_pos_centroid <- sample(1:len_centroids, 1)
  rand_pos_sismo <- sample(1:len_sismos, 1)
  
  centroids[[rand_pos_centroid]][[2]]=sismos[[rand_pos_sismo]][[2]]
  centroids[[rand_pos_centroid]][[3]]=sismos[[rand_pos_sismo]][[3]]
  
  return (centroids)
}

###################################################################################

tusDatos <- read.table(file.choose(), skip = 0, header = TRUE, sep =',')

# Initialize `sismos`
sismos <- list()
# Creando y llenando la estructura de datos listas de listas `sismos`
for(i in 1:nrow(tusDatos)) {
  sismos[[i]] <- list(tusDatos[i,1],tusDatos[i,2],tusDatos[i,3],tusDatos[i,4],tusDatos[i,5],tusDatos[i,6],tusDatos[i,7],tusDatos[i,8],tusDatos[i,9])
}

###Inicialización centroides
cent <- initialSolution(sismos, 3)
lista_promedio <- calculateAVGdistance(cent,sismos)
