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

ILS <- function(max_iteraciones,funcion_vecindad){
  #' Inicializando soluciones inicial           
  centroids <- initialSolution(sismos, num_centroids=max_clusters)
  sismos,centroids <- kmedians(sismos,centroids)
  dist <- objectiveFunction(sismos,centroids)
  for (i in 1:max_iteraciones) {
    if(funcion_vecindad == 1){
      centroids_new = randomSwap(centroids, sismos)
    }else{
      centroids_new = averageNeighbour(centroids, sismos)
    }
    sismos_new <- sismos
    sismos_new, centroids_new = kmedians(sismos_new, centroids_new)
    dist_new <- objectiveFunction(sismos_new,centroids_new)
    if (dist_new < dist){
      sismos <- sismos_new
      centroids <- centroids_new
    }
  }
  return (sismos, centroides)
}

distan <- objectiveFunction(sismos2,centroids)