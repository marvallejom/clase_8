#==============================================================================#
# Autor(es): Eduard Martinez
# Colaboradores: 
# Fecha creacion: 10/08/2019
# Fecha modificacion: 10/03/2021
# Version de R: 4.0.3.
#==============================================================================#

# intial configuration
rm(list = ls()) # limpia el entorno de R
pacman::p_load(tidyverse,data.table) # cargar y/o instalar paquetes a usar

#-----------------------#
# 1. Funcion 'function' #
#-----------------------#

#### 1.0.1 Podemos obtener ayuda adiccional aqui
browseURL(url = "https://www.datamentor.io/r-programming/function/", browser = getOption("browser")) # Help 1
browseURL(url = "https://swcarpentry.github.io/r-novice-inflammation/02-func-R/", browser = getOption("browser")) # Help 2

#### 1.1. Ejemplos de una funcion
f_caracter <- function(x){ 
              x = x %>% toupper(.) %>% trimws(.)
return(x)
}
f_caracter(" Buenos Dias ")

#### 1.1.1. Apliquemos la funcion f_caracter sobre un vector de caracteres
vector_caracteres <- c(" Eduard Martinez "," Taller de R y estadistica "," Hola ","Buenas tardes ")

# Usando un loop
for (i in vector_caracteres){
     print(i)
     print(f_caracter(i))  
}

for (i in 1:length(vector_caracteres)){
     print(i)
     print(f_caracter(vector_caracteres[i]))  
}

# Usando lapply
lapply(X = vector_caracteres,function(caracter) f_caracter(x = caracter))

#### 1.2.  Sino indico la opcion return(), la funcion me devuelve el elemento que se escuentra en la ultima linea del cuerpo
betas_ols <- function(x,y){ 
          vector_unos <- matrix(data = 1, nrow = length(y) , ncol = 1)
          x <- cbind(vector_unos,x)
          beta <- solve(t(x)%*%x)%*%t(x)%*%y
beta   
}

"Probemos nuestra funcion, primero generemos una matrix con las variables independientes y un vector con la variable dependiente"
indep <- matrix(rnorm(20,100,50), nrow = 10, ncol = 2)
depen <- matrix(100:109, nrow = 10, ncol = 1)

"Ahora vamos a hacer la estimacion usando la funcion que creamos"
betas_ols(x = indep, y = depen)

"Verifiquemos los resultados usando la funcion lm() de el paquete 'stats'"
lm(depen ~ indep)

#------------------------------------------------#
# 2. Funciones de control dentro de una funcion. #
#------------------------------------------------#
browseURL(url = "https://mauricioanderson.com/curso-r-debugging/", browser = getOption("browser")) # Muy bueno!

#### 2.1. Opcion warnings
funcion_rc <- function(numero){
          if (numero >= 0){
          resultado <- sqrt(numero)  
          return(resultado)
          }  
}
funcion_rc(25)
funcion_rc(0)

"Es importante indicarle a la fucnion si se da el caso de no cumplir una de las condiciones, es decir, 
se debe indicar que debe hacer la funcion en caso de que no se cumpla una condicion excluyente.
Por ejemplo apliquemos la funcion a un numero negativo"
funcion_rc(-100)

"Reescribamos la funcion"
funcion_rc <- function(numero){
  
            # Si es mayor o igual a cero aplicar la raiz cuadrada
            if (numero >= 0){
                resultado <- sqrt(numero)  
            return(resultado)
            }  
            
            # Si es menor a cero
            if (numero < 0){
                warning(paste0("Advertencia! ",numero," es un numero negativo, se reporta la raiz cuadrada del valor absoluto (",abs(numero),")"))
                numero <- abs(numero) %>% sqrt(.)  
            return(numero)
            }
}
funcion_rc(-100)
funcion_rc(0)

#### 2.1. Opcion stop()
"Cuando obtenemos un 'Error' en la consola despues de eejcutar una funcion, nos indica que no 
aplico la funcion y debemos verificar que es lo que no esta bien"
"Reescribamos la funcion"
funcion_palabra <- function(palabra){
  
                    # Si es un caracter
                    if (is.character(palabra) == TRUE){
                        result <- trimws(palabra) %>% toupper(.)  
                        return(result)
                    }  
                    
                    # Si no es un caracter
                    if (is.character(palabra) == FALSE){
                        stop(paste("Advertencia! el elemento",palabra," no es un elemento de tipo caracter"))
                    }
}
funcion_palabra("hola")
funcion_palabra(100)


#------------------------------------------------#
# 3. Funciones de control dentro de una funcion. #
#------------------------------------------------#
browseURL(url = "https://mauricioanderson.com/curso-r-debugging/", browser = getOption("browser")) # Muy bueno!

### 3.1. Veamos traceback()
indep <- matrix(rnorm(20,100,50), nrow = 10, ncol = 2)
depen <- matrix(100:109, nrow = 10, ncol = 2)

'Ejecutemos nuestra funcion'
betas_ols(x = indep, y = depen)
traceback() # Me muestra la lista de funciones que se ejecutan hasta antes de cometerse el error

### 3.2. Veamos debug() y undebug() revisen el enlace que les dejo arriba
browseURL(url = "https://mauricioanderson.com/curso-r-debugging/", browser = getOption("browser")) 


#--------------------------#
# 4. Construir una funcion #
#--------------------------#

### Construir en la clase esta funcion paso a paso...
base_n = readxl::read_excel(path = 'data/input/homicidios-2018.xls') %>% 
         subset(is.na(`...2`)==F)
names_n <- base_n[1,] %>% as.character() %>% tolower() %>% gsub(" ","",.)
colnames(base_n) <- names_n
base_n <- base_n[2:nrow(base_n),] %>% subset(is.na(departamento)==F)
base_n

### Como cargar archivos de SIEDCO
cargar_siedco <- function(path_file){
                  base = readxl::read_excel(path = path_file) %>% 
                         subset(is.na(`...2`)==F)
                  names <- base[1,] %>% as.character() %>% tolower() %>% gsub(" ","",.)
                  colnames(base) <- names
                  base <- base[2:nrow(base),] %>% subset(is.na(departamento)==F)
                 return(base)
}
file_archivos <- list.files("data/input/") %>% paste0("data/input/",.)
lista_data = lapply(file_archivos,function(x) cargar_siedco(path_file = x)) 
database = lista_data %>% data.table::rbindlist(use.names = T,fill = T)
  
#----------#
# 5. Bonos #
#----------#

### Revisen estas funciones en la casa
"Carguemos algunas funciones que estan en el script Funciones.R"
source("scr/aux_functions.R") # Asi se lee un script de 

# Apliquemos la funcion caracter_vector   
caracter_vector("Hola",3)
caracter_vector("Hola",3)
caracter_vector("Hola",4)
caracter_vector("Hola",5)

# Usemos la base de datos de hurtos
hurtos <- readRDS("data/output/hurtos 2018.rds")
str(hurtos)

"Cuales son los valores no numericos de la variable edad"
nonumedad <- extrac_nonumeric_vector(hurtos[,"edad"])
nonumedad
nonumfecha <- extrac_nonumeric_vector(hurtos[,"fecha"])
nonumfecha

# Hagamos unas decsriptivas de la base de datos
descriptiva <- descriptive_function(hurtos)
descriptiva
