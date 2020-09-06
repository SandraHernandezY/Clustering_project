library(cluster)
library(factoextra)

instances<-3
time_instances <-list()             # Tiempo que tarda la ejecucion de una instancia

tusDatos <- read.table(file.choose(), skip = 0, header = TRUE, sep =',')

sismos <- list()
# Creando y llenando la estructura de datos listas de listas `sismos`
                   # latitud          longitud 
sismos <-rbind(cbind(tusDatos[,2],tusDatos[,3])) 

# Specify column and row names
colnames(sismos) <- c("latitud ", "longitud") 

#LLamado de la función:

#'x: una matriz de datos numéricos o data frame, cada fila corresponde: a una observación y cada columna 
#'corresponde a una variable. Se permiten valores perdidos (NA).
#'
#'k: the number of clusters.
#'
#'métrica: métrica de distancia que se utilizará. Las opciones disponibles 'son "euclidiana" y "manhattan".
#'Las distancias euclidianas son la raíz de la suma de cuadrados de las diferencias, y las distancias de
#'Manhattan son la suma de las diferencias absolutas. Leer más sobre medidas de distancia (Capítulo).
#'Tenga en cuenta que la distancia de Manhattan es menos sensible a los valores atípicos.
#'
#'
#'stand: valor lógico; si es verdadero, las variables (columnas) en x se estandarizan antes de calcular las
#'diferencias. Tenga en cuenta que se recomienda estandarizar las variables antes de agruparlas.
#'
#'samples: número de muestras que se extraerán del conjunto de datos. El valor predeterminado es 5, pero se
#'recomienda un valor mucho mayor.
#'
#'pamLike: lógica que indica si se debe utilizar el mismo algoritmo en la función pam ().
#' Esto debería ser siempre TRUE.
#'

for (inst in 1:instances) {
  
#--------- INICIO --------------
start_time = Sys.time()
clara.res <- clara(sismos, 5, metric = "manhattan", stand =TRUE, samples = 5, pamLike = TRUE)
end_time = Sys.time()
#------------ FIN --------------
total_time = end_time - start_time
total_time = as.numeric(total_time, units = "secs")
print(total_time)
time_instances <<- append(time_instances, total_time)
#------ fin instancia------------

}

#------------------------ Visualizar Datos + Graficos --------------------------
# Compute CLARA metrica de distancia?  clara.res <- clara(df, 2, samples = 50, pamLike = TRUE)

print(clara.res)

# Medoids
clara.res$medoids

# Clustering
head(clara.res$clustering, 10)

#require(cluster)
#pam.res <- pam(iris.scaled, 3)
# Visualize pam clustering              frame.type = "norm"
fviz_cluster(clara.res, geom = "point", frame.type = "t")

