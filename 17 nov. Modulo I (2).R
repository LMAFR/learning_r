################################MODULO I PRIMEROS PASOS CON R (II): manejo de datos y bases de datos

#***************************************Otros objetos de R

#FUNCIONES: objetos de R con los que podemos operar
###es posible crear funciones propias usando el lenguaje de programación
###o utilizar funciones ya creadas
a <- -1:4
sqrt(a)     # Raiz cuadrada de cada numero de a

#PAQUETES
###¿Donde podemos encontrar las funciones ya creadas?
###en la instalacion base de R o en PAQUETES
###los paquetes son conjuntos de funciones. Para su uso:

#1. descargar el paquete e instalarlo
install.packages("nombre del paquete")   
#2.cargar el paquete en la memoria
library("nombre del paquete")             

###O en Menu "packages"-> Install, escribir el paquete e instalarlo
#En la consola se muestra el progreso, cualquier error que pueda surgir
#o paquete que debe instalarse previamente para que el paquete que queremos funcione

###Algunos paquetes utiles:
install.packages("stats") #funciones estadisticas
install.packages("dplyr") #manipulación y operaciones con data frames
install.packages("ggplot2") #creacion de graficos avanzados
install.packages("tidyverse") #conjunto de paquetes para manejar, transformar, and visualizar datos
#https://www.rdocumentation.org/packages/tidyverse/versions/1.3.0
library("stats")
library("dplyr")
library("ggplot2")
library("tidyverse")

#**********************************************************DIRECTORIO

###Es necesario saber en qué directorio estamos trabajando (sobre todo para importar y exportar archivos)
getwd()#directorio de trabajo actual
#o simplemete mirar en la Consola

#para modificarlo
setwd("ruta del directorio") # Lo que se suele hacer es copiar la ruta que nos interesa y poner las barras (\) hacia la derecha (/) antes de darle a enter.
#o poner un nuevo Default directory: "Tools" -> "Global Options"-> "General" ->"R Sessions" 

#Puedo listar todos los elementos que existen en el WD con las siguientes funciones
dir()#ver archivos del directorio
list.files()#ver archivos del directorio
#y la flecha de al lado de 'Console' nos da acceso a esos archivos

file.info("correos.docx") #me permite obtener informacion de un archivo como su peso o fecha de creacion, por ejemplo
?file.info # para ver los detalles de que es cada columna

#***********************************************************IMPORTAR DATOS

### A la hora de trabajar con bases de datos en R es MUY imporante preparar los datos previamente
###Para importar: podemos usar los menus: Environment -> Import Dataset
read_csv(file.choose())
###o el codigo (permite mayor control)

###Existen paquetes especificos para cada tipo de archivo
## ver http://www.statmethods.net/input/importingdata.html
library(Hmisc)#SPSS
library(readxl)#Excel...

#CONSEJO! antes de importar a R es mejor transformar los datos a un 
#archivo de datos separado por comas (.csv) o tabulador (.txt) y usar read.csv o read.delim para importar
#(si se usa la coma como dicimal: usar read.delim2 o read.csv2)
# ver http://cran.r-project.org/doc/manuals/r-release/R-data.html

#Leemos un archivo de datos
#si importo desde la web
datos.covid <-read.csv("https://cnecovid.isciii.es/covid19/resources/agregados.csv", header =T)
#si importo desde mi carpeta
Ejemplo_CSV <- read.csv("agregados.csv", header = TRUE)
Ejemplo_CSV <- as.data.frame(Ejemplo_CSV) # Lo convierte en data.frame
Ejemplo_CSV <- as.data.frame(read.csv("agregados.csv", header = TRUE)) #Hace lo mismo que la anterior, por motivos obvios.

library(readxl)
help(read_excel)#leer todas las opciones que nos da esta funcion
excel <- read_excel('nombrearchivo.xlsx',sheet = "puedo elegir una hoja especifica", range = cell_cols("tambien rango de celdas"), col_names = TRUE)
excel <- read_excel('Gasto_de_los_turistas_segun_destino_principal.xlsx',col_names = TRUE)
#aparece en Console un texto en rojo de New Names
View(excel) #nos damos cuenta que la primera fila que deberia tener los nombres de las columnas estaba vacia
#Muchas veces sucede que al importar un archivo necesitamos excluir filas iniciales porque no tienen info de valor
#Para ello existe el parametro skip, que me permite excluir la cantidad de filas que desee

#Probamos de nuevo
Archivo_excel <- read_excel("Gasto_de_los_turistas_segun_destino_principal.xlsx", col_names = TRUE, skip = 1)
#en Console no ha aparecido  mensaje de error
View(Archivo_excel) # Ahora aparecen los nombres de variables donde tenían que aparecer porque nos hemos saltado (skip) la primera fila en la que no se encontraban.

#para inspeccionar los datos importados:
head(Archivo_excel)
tail(Archivo_excel)#no nos es muy util en este archivo
head(Ejemplo_CSV)
head(Ejemplo_CSV, n=10)# le podemos espepecificar el nº de lineas que queremos ver
tail(Ejemplo_CSV)

length(Archivo_excel)
length(Ejemplo_CSV)
dim(Archivo_excel)

str(Archivo_excel)#muestra todas las variables y de que tipo son
summary(Archivo_excel)#muestra los descriptivos basicos de todas las variables. También muestra si hay NA's en cada variable y cuántos son.
summary(Ejemplo_CSV) #¿que ocurre con estos descriptivos en las variables categoricas? 
                     # Que en lugar de darte valores estadísticos, te da la longitud (número de elementos), la clase y el modo.

#habria que mirar que cada variable la toma como debe (numerica, caracter...)
#Archivo_excel$
class(Archivo_excel$Gasto_2004) # Usamos el $ para acceder a cada columna del dataset.
str(Archivo_excel$Gasto_2004)
levels(Archivo_excel$Gasto_2004)   #es tan solo un vector con numeros por lo que no tiene niveles
#¿que variable podria tener niveles?¿por que? Un factor, porque es un objeto categórico.

summary(Archivo_excel$Gasto_2004)

#¿cómo podemos saber si hay datos perdidos en mi base?

#Otra forma de importar datos es copiandolos desde el archivo original y usando la opcion 'clipboard'
my_data <- read.table(file = "clipboard")#nos da error porque no le hemos dicho como es la estructura de nuestros datos
my_data <- read.table(file = "clipboard",
                      sep = "\t", header=TRUE) # Esta opción te copia en una tabla lo que tienes copiado en el portapapeles.
# Los .csv se separan por comas y los Excel se separan por tabulador (\t en la línea anterior).


#***********************************MODIFICAR Y EXPORTAR DATOS
###Hemos visto con tail() que al final de los datos del COVID hay comentarios que no nos interesan
tail(Ejemplo_CSV) 

#asi que eliminamos esas filas
(datoscovid<-Ejemplo_CSV[1:1729,])
# otra forma de hacerlo es esta:
# (datoscovid<-Ejemplo_CSV[-(1730:length(datoscovid$CCAA)),])
datoscovid<-Ejemplo_CSV[-(1730:1738),] # Hemos sustituido la anterior por esta porque nos da un error y la profe no sabe por qué ahora mismo. (Creo que ya arreglé yo el error)

### CREAMOS UN OBJETO solo CON LOS DATOS COVID DE LA COMUNIDAD DE MADRID
# DEBEMOS FILTRAR LAS FILAS CUYA VARIABLE CCAA SEA CM:
CM<-datoscovid[(datoscovid$CCAA=="CM"),] 
#(ver otro tipo de operadores logicos en PPT)

#GUARDAMOS EL RESULADO EN un nuevo ARCHIVO llamado "datoscovidCM.csv"
write.csv(CM, "C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/Día 2/datoscovidCM.csv")

CM2<-read.csv("datoscovidCM.csv", header=T,sep=',')#comprobamos que se importa bien
#CUIDADO: mirar en el global environment ¿que ha pasado?

# vemos que tiene una variable más que CM y es porque está tomando el nº de fila 
#como una columna mas. Para quitarla podemos hacer simplemente lo siguiente:
CM2<-CM2[,-1]

#------------------------------------
###Tenemos el Global environment demasiado lleno
ls()#ver los objetos del epacio de trabajo
rm(CM)#borrar un objeto que ya no quedamos

rm(list=ls()) #borrar todos los objetos del espacio de trabajo

