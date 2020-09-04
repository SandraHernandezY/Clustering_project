library(ggplot2)

objectiveFunction <- function(sismos, centroides){
  totalDist = 0
  for (i in 1:length(sismos)) {
    for (j in 1:length(centroides)) {
      if(sismos[[i]][[9]] == j){
        d <- distHaversine(c(sismos[[i]][[2]],sismos[[i]][[3]]),c(centroids[[j]][[2]],centroids[[j]][[3]]),r= 6371.0) 
        totalDist = totalDist + d
      }
    }
  }
  return (totalDist)
}

ILS <- function(max_iteraciones,funcion_vecindad,sismos,centroids){
  #' Inicializando soluciones inicial           
  dist <- objectiveFunction(sismos,centroids)
  for (i in 1:max_iteraciones) {
    if(funcion_vecindad == 1){
      centroids_new = randomSwap(centroids, sismos)
    }else{
      centroids_new = averageNeighbour(centroids, sismos)
    }
    sismos_new <- sismos
    lista_new <- kmedians(sismos_new, centroids_new)
    sismos_new <- lista_new[[1]]
    centroids_new = lista_new[[2]]
    dist_new <- objectiveFunction(sismos_new,centroids_new)
    if (dist_new < dist){
      sismos <- sismos_new
      centroids <- centroids_new
    }
  }
  final  <- list(sismos,centroids)
  return (final)
}

centroids <- initialSolution(sismos, num_centroids=3)
lista <- kmedians(sismos,centroids)

sismos <- lista[[1]]
centroids <- lista[[2]]
finales <- ILS(50, 1,sismos,centroids)
sismos_finales <- finales[[1]]
centroides_finales <- finales[[2]]


###Plotting
long <- list()
lat <- list()
cluster <- list()
for (i in 1:length(sismos)) {
  lat <- append(lat, sismos_finales[[i]][[2]])
  long <- append(long, sismos_finales[[i]][[3]])
  cluster <- append(cluster, sismos_finales[[i]][[9]])
}

#hola <- list(1,2,3,4,5)
#chao <- list(6,7,8,9,10)
#juju <- c(hola,chao)
#matriz <- matrix(juju, nrow = 2, dimnames = list(c(),c("Col1","Col2","Col1","Col2","Col5")))

x <- matrix(c(lat,long,cluster), nrow = length(sismos), dimnames = list(c(),c("Lat","Long","Cluster")))
x_new <- as.data.frame(x)
x_new$Lat = as.numeric(x_new$Lat)
x_new$Long = as.numeric(x_new$Long)
x_new$Cluster = as.numeric(x_new$Cluster)
x_new$Cluster = as.factor(x_new$Cluster)
#ggplot(data = x_new, aes(Lat, Long))
#+ 
 # geom_point(aes(Lat, Long, colour = factor(Cluster)))

ggplot(x_new, aes(x = Long, y = Lat, colour = Cluster)) + geom_point()



#plot(x = sismos_finales[[i]][[2]], y = sismos_finales[[i]][[3]])
