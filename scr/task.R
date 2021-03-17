#==============================================================================#
# Autor(es): Eduard Martinez
# Colaboradores: 
# Fecha creacion: 17/03/2019
# Fecha modificacion: 17/03/2021
# Version de R: 4.0.3.
#==============================================================================#

# intial configuration
rm(list = ls()) # limpia el entorno de R
pacman::p_load(tidyverse,data.table,readxl) # cargar y/o instalar paquetes a usar

#----------------------------------#
# Importar archivos usando un loop #
#----------------------------------#

# crear vector con ruta de objetos a cargar
list.files(path = "data/input/" , full.names = T)
files = list.files(path = "data/input/" , full.names = T)
  
# lista para almacenar bases de datos
lista_data = list()
lista_data

# Loop
conteo = 1 # Para contar numero iterraciones
for (i in files){
     lista_data[[conteo]] = read_excel(path = i)
     conteo = conteo + 1
}

# exportar lista
saveRDS(lista_data,"data/output/lista siedco.rds")

#----------------------------------#
# Importar archivos usando un loop #
#----------------------------------#

# Importar archivo "lista siedco.rds" de data/output
ldata = readRDS("data/output/lista siedco.rds")

# Verificar visualmente los datos
ldata[[1]] #Muestra elemento 1
ldata[[10]] #Muestra elemento 10

df1=ldata[[1]]
df10=ldata[[10]]

View(df10)
# Limpiar una base de datos
##Quitar las filas de NA
df_i = ldata[[4]]
df_i=subset(df_i,is.na(...2)==F)

##Hacer que mis nombres sean la nueva fila 1
colnames(df_i)=df_i[1,]
## Ahora elimino la primea fila 
df_i = df_i[-1,]


# Generalizar el paso anterior en una funcion
f_clean = function(i){
  df_i=ldata[[i]]
  df_i=subset(df_i, is.na(...2)==F) #elimino obs no relevantes
  colnames(df_i)=df_i[1,]
  df_i=df_i[-1,]
return(df_i)
}

#Aplicar la funcion 
data = lapply(1:14, function(x) f_clean(i=x))

#Veamos elemento de la lista
dc1 = data[[1]]
dc10 = data[[10]]

#Aplicar un dataframe
dataframe = rbindlist(l = data,use.names = ,fill=T)




