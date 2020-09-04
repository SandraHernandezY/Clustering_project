library(ggplot2)
library(ggmap)

time_instances <-list()             # Tiempo que tarda la ejecucion de una instancia
inst_best_objetive <-list()         # Mejores objetivos por instancia
inst_best_objetive_iter <-list() # Mejores objetivos por iteracion

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


ILS <- function(max_iteraciones,funcion_vecindad,sismos, max_clusters,medians){
  # Inicializando soluciones inicial
  centroids <- initialSolution(sismos, num_centroids=max_clusters)
  lista <- kmedians(sismos,centroids,medians)
  sismos <- lista[[1]]
  centroids <- lista[[2]]           
  dist <- objectiveFunction(sismos,centroids)
  
  bestObjective= dist
  objectives <- list()
  objectives <- append(objectives,dist)  
  
  for (i in 1:max_iteraciones) {
    
    if(funcion_vecindad == 1){
      centroids_new = randomSwap(centroids, sismos)
    }else{
      centroids_new = averageNeighbour(centroids, sismos)
    }
    
    sismos_new <- sismos
    lista_new <- kmedians(sismos_new, centroids_new,medians)
    sismos_new <- lista_new[[1]]
    centroids_new = lista_new[[2]]
    dist_new <- objectiveFunction(sismos_new,centroids_new)
    
    if (dist_new < dist){
      sismos <- sismos_new
      centroids <- centroids_new
      bestObjective= dist
      objectives <- append(objectives,dist)
    }
  }
  
  inst_best_objetive_iter<<- append(inst_best_objetive_iter, objectives)
  inst_best_objetive<<- append(inst_best_objetive, bestObjective)
  
  final  <- list(sismos,centroids)
  return (final)
}

#--------- INICIO ---------
start_time = Sys.time()
finales <- ILS(50, 2,sismos,5,1)
end_time = Sys.time()
#------------ FIN --------------

total_time = end_time - start_time
total_time = as.numeric(total_time, units = "secs")
print(total_time)

sismos_finales <- finales[[1]]
centroides_finales <- finales[[2]]















###--------------Plotting------------------
long <- list()
lat <- list()
cluster <- list()
for (i in 1:length(sismos)) {
  lat <- append(lat, sismos[[i]][[2]])
  long <- append(long, sismos[[i]][[3]])
  cluster <- append(cluster, sismos[[i]][[9]])
}

x_1 <- matrix(c(lat,long,cluster), nrow = length(sismos), dimnames = list(c(),c("Lat","Long","Cluster")))
x_new_1 <- as.data.frame(x_1)
x_new_1$Lat = as.numeric(x_new_1$Lat)
x_new_1$Long = as.numeric(x_new_1$Long)
x_new_1$Cluster = as.numeric(x_new_1$Cluster)
x_new_1$Cluster = as.factor(x_new_1$Cluster)

ggplot(x_new_1, aes(x = Long, y = Lat, colour = Cluster)) + geom_point()


###Plotting
long <- list()
lat <- list()
cluster <- list()
for (i in 1:length(sismos)) {
  lat <- append(lat, sismos_finales[[i]][[2]])
  long <- append(long, sismos_finales[[i]][[3]])
  cluster <- append(cluster, sismos_finales[[i]][[9]])
}

x <- matrix(c(lat,long,cluster), nrow = length(sismos), dimnames = list(c(),c("Lat","Long","Cluster")))
x_new <- as.data.frame(x)
x_new$Lat = as.numeric(x_new$Lat)
x_new$Long = as.numeric(x_new$Long)
x_new$Cluster = as.numeric(x_new$Cluster)
x_new$Cluster = as.factor(x_new$Cluster)

ggplot(x_new, aes(x = Long, y = Lat, colour = Cluster)) + geom_point()

