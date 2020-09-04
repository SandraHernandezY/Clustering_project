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

centroids <- initialSolution(sismos, num_centroids=5)
lista <- kmedians(sismos,centroids)
print(lista)
sismos <- lista[[1]]
centroids <- lista[[2]]
finales <- ILS(3, 1,sismos,centroids)
sismos_finales <- finales[[1]]
centroides_finales <- finales[[2]]


###Plotting
#long <- list()
#lat <- list()
#cluster <- list()
#for (i in 1:lenght(sismos)) {
#  lat <- append(lat, sismos_finales[[i]][[2]])
#  long <- append(long, sismos_finales[[i]][[3]])
#  cluster <- append(cluster, sismos_finales[[i]][[9]])
#}

#plot(x = sismos_finales[[i]][[2]], y = sismos_finales[[i]][[3]])
