#install.packages('readr')
#library(readr)

#Option 1
#file.choose(): pantalla emergente para selecionar archivo "Sismos_Ecuador"
# puede ser remplazado por la ruta del archivo "Sismos_Ecuador"

data <- read.csv(file.choose(),skip = 0)

#data
#data[1]      retorna la primera columna
#data[1,]     retorna la primera fila
#ncol(data)   retorna la cantidad de columnas
#length(data) retorna la cantidad de columnas
#nrow(data)   retorna la cantidad de filas
#data[[1]]    retorma TODA la columna
#sismos[i]    Acceso a cada sismo i con todas sus caracteristicas
#sismos[[i]][[j]] Acceso a la elem j del sismo i

#Option 2
tusDatos <- read.table(file.choose(), skip = 0, header = TRUE, sep =',')


# Initialize `sismos`
sismos <- list()
# Creando y llenando la estructura de datos listas de listas `sismos`
for(i in 1:nrow(tusDatos)) {
  sismos[[i]] <- list(tusDatos[i,1],tusDatos[i,2],tusDatos[i,3],tusDatos[i,4],tusDatos[i,5],tusDatos[i,6],tusDatos[i,7],tusDatos[i,8])
}

