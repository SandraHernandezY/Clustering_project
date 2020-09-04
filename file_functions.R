#data <- read.csv(file.choose(),skip = 0)

#data
#data[1]      retorna la primera columna
#data[1,]     retorna la primera fila
#ncol(data)   retorna la cantidad de columnas
#length(data) retorna la cantidad de columnas
#nrow(data)   retorna la cantidad de filas
#data[[1]]    retorma TODA la columna
#------------------------------------------------------
#sismos[i]          Acceso a cada sismo i con todas sus caracteristicas
#sismos[[i]][[j]]   Acceso a la elem j del sismo i
#length(sismos)     retorna cantidad de filas 
#length(sismos[[1]])retorna cantidad de columnas

#Option 2
tusDatos <- read.table(file.choose(), skip = 0, header = TRUE, sep =',')

# Initialize `sismos`
sismos <- list()
# Creando y llenando la estructura de datos listas de listas `sismos`
for(i in 1:nrow(tusDatos)) {
  sismos[[i]] <- list(tusDatos[i,1],tusDatos[i,2],tusDatos[i,3],tusDatos[i,4],tusDatos[i,5],tusDatos[i,6],tusDatos[i,7],tusDatos[i,8],tusDatos[i,9])
}

#PRUEBA: centrides <- initialSolution(sismos, num_centroids=3)
#RESPUESTA: [[1]] 
#           [[1]][[1]] [1] 1
#           [[1]][[2]] [1] 3.402023
#           [[1]][[3]] [1] 0.88
#           [[2]]
#           [[2]][[1]] [1] 2
#           [[2]][[2]] [1] 10
#           [[2]][[3]] [1] 0.22
#           [[3]]
#           [[3]][[1]] [1] 3
#           [[3]][[2]] [1] 0.4745412
#           [[3]][[3]] [1] -0.25
