library(ggpubr)
library(rworldmap) 
library(reshape2)

clustering_plot <- function(sismos){
  long <- list()
  lat <- list()
  cluster <- list()
  
  world <- getMap(resolution = "low")
  countries <- c("Ecuador")
  world_ec <- world[world@data$ADMIN %in% countries, ]
  
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
  
  (with_world <- ggplot() +
      geom_polygon(data = world_ec, 
                   aes(x = long, y = lat, group = group),
                   fill = NA, colour = "black") + 
      geom_point(data = x_new,  # Add and plot species data
                 aes(x = Long, y = Lat, 
                     colour = Cluster)) +
      coord_quickmap() +  # Prevents stretching when resizing
      theme_classic() +  # Remove ugly grey background
      xlab("Longitude [GD]") +
      ylab("Latitude [GD]") + 
      ylim(-2,1.5) +
      xlim(-82,-78) + 
      guides(colour=guide_legend(title="Clusters"))) +
    ggtitle("Clustering de sismos ubicados en Ecuador")
  
  #ggplot(x_new, aes(x = Long, y = Lat, colour = Cluster)) + geom_point()
}

objectivesIterations_plotting <- function(list_objectives){
  list_objectives <- as.numeric(list_objectives)
  plot(list_objectives,main="Valor objetivo por cada instancia ejecutada",xlab="Instancia", ylab="Valor Objetivo [km]")
}

objectivesInstances_plotting <- function(list_objectives){
  x <- data.frame(iteraciones=c(1:length(list_objectives[[1]])))
  for (i in 1:length(list_objectives)) {
    x <- cbind(x,i=as.numeric(list_objectives[[i]]))
  }
  columns <- c("iteraciones",1:length(list_objectives))
  colnames(x)<-columns
  x_new <- melt(x, id.vars = 'iteraciones',variable.name = 'Instancia')
  ggplot(x_new, aes(iteraciones,value)) + 
  geom_line(aes(colour = Instancia))+
  xlab("Iteraciones") +
  ylab("Valor Objetivo [km]") +
  ggtitle("Valores objetivos de cada iteración en las instancias.")
}
