clustering_plot <- function(sismos){
  long <- list()
  lat <- list()
  cluster <- list()
  
  for (i in 1:length(sismos)) {
    lat <- append(lat, sismos[[i]][[2]])
    long <- append(long, sismos[[i]][[3]])
    cluster <- append(cluster, sismos[[i]][[9]])
  }
  
  x <- matrix(c(lat,long,cluster), nrow = length(sismos), dimnames = list(c(),c("Lat","Long","Cluster")))
  x_new <- as.data.frame(x)
  x_new$Lat = as.numeric(x_new$Lat)
  x_new$Long = as.numeric(x_new$Long)
  x_new$Cluster = as.numeric(x_new$Cluster)
  x_new$Cluster = as.factor(x_new$Cluster)
  
  ggplot(x_new, aes(x = Long, y = Lat, colour = Cluster)) + geom_point() + ylim(0,20)
}

#objetivesIteration_plotting <- funcion(list_objectives){
#  ggplot(list_objectives, aes(x = iteraciones, y = valor objetivo, col = myWords)) +
#    geom_point()
  
#  ggplot(x_new, aes(x = Long, y = Lat, colour = Cluster)) + geom_point()
  
#}


