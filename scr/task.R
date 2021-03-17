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


# Verificar visualmente los datos


# Limpiar una base de datos


# Generalizar el paso anterior en una funcion






