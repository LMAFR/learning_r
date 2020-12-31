################################MODULO III Estadística con R (I)

setwd("C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/Día 5 - Curvas de ajuste")
datos.covid <- read.csv("agregados.csv", header = TRUE)
setwd("C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/Día 5 - Curvas de ajuste")
encuesta <- read.table("encuesta.dat", header=T, sep="\t",dec=',')

#*****************************Descripción estadística de las variables de un conjunto de datos

###Variables categóricas: frecuencias
str(encuesta)#vamos a ver que variable de nuestros datos era categorica
genero<- factor(encuesta$Sexo, levels = c(1,2), labels = c("Hombre", "Mujer"))
str(genero)
table(genero)
barplot(table(genero))

###Variables cuantitativas
table(encuesta$Nota_PAU) #podemos sacar mucha mas informacion que las frecuencias

#MEDIDAS CENTRALES
#install.packages("DescTools")

library(DescTools) #Librería para poder usar una función que nos devuelva la moda.

Mode(encuesta$Nota_PAU)   #valor mas frecuente (esto es, la moda) y frecuencia de ese valor (ver table())
median(encuesta$Nota_PAU) #mediana (ver boxplot)
mean(encuesta$Nota_PAU)   #media

#DISPERSION
#¿por que es importante? Ejemplo
edad1 <- c(49, 49 ,49, 49, 50, 50, 51, 51, 51, 51)
edad2 <- c(42 ,44 ,46, 48, 50, 50, 52, 54, 56, 58)
edad3 <- c(10 ,20 ,30, 40, 50, 50, 60, 70, 80, 90)
mean(edad1)
mean(edad2)
mean(edad3)
#segun la media esas tres variables serian iguales, pero tienen valores distintos

matrix(c(var(edad1), sd(edad1),var(edad2), sd(edad2),var(edad3), sd(edad3)), 3,2,
       dimnames = list(c("edad 1", "edad 2","edad 3"),
                       c("Varianza","Desviacion Tipica")),byrow=T)

# ¡Cuidado! Según dice una compañera, la función var no calcula la varianza, sino la cuasivarianza (divide entre N-1, no entre N).

#¿que variable tiene mas dispersion?

# A mayor varianza, mayor dispersión en los valores. Se suele usar la desviación 
# típica como medida del error porque tiene las mismas dimensiones que los datos para los que calculas la varianza.

#FORMA
hist(encuesta$Nota_PAU,
     xlab   = "Nota",
     ylab   = "Frecuencia",
     main   = "Notas obtenidas en la PAU",
     col    = "dodgerblue",
     border = "darkorange")
hist(encuesta$NotaEsperada,
     xlab   = "Nota esperada",
     ylab   = "Frecuencia",
     main   = "Nota esperada en la PAU",
     col    = "dodgerblue",
     border = "darkorange")#asimetria negativa
hist(encuesta$Edad,
     xlab   = "Edad",
     ylab   = "Frecuencia",
     main   = "Edad",
     col    = "dodgerblue",
     border = "darkorange")#asimetria positiva

# Distribución normal o gaussiana: hemos explicado cualitativamente lo que es (forma de la curva creada).

# Curtosis: hace referencia al grado de apuntamiento (como de afilado es el pico) de la curva.

#CUANTILES
library(ggplot2)
box <-ggplot(encuesta, aes(Nota_PAU))
box+
        geom_boxplot()+
        labs(x = "Nota en la PAU")

#Funciones que agrupan DESCRIPTIVOS ESTADISTICOS
summary(encuesta$Nota_PAU)
install.packages("psych")
library(psych) # Nos permite aplicar las funciones de las siguientes líneas.
describe(encuesta$Nota_PAU) # Es como un summary, pero mejor.
source('descriptivos.r') # También podríamos usar esta función creada por la profesora que vimos el otro día.
descriptivos (encuesta$Nota_PAU)

by(encuesta[,"Edad"], encuesta[,"Sexo"], describe)  #Uso de la función describe sobre los valores de Edad divididos para cada Sexo.

#*********************************Estandarizado y normalizado de datos
#*¿por que tipificar?
#*1. para tener variables de la misma metrica, p.ej. ¿se pueden comparar kilos con cm?
#*2. para tener variables con el mismo centro
#*3. para tener variables con la misma dispersion
#ejemplo: millones ganados por tres empresas distintas a lo largo de 12 años

ganancias<- data.frame(empresaA= c(4.0, 4.8, 5.0, 5.2, 5.6, 6.0, 6.0, 6.3,6.7, 7.0, 7.4, 8.0),
empresaB = c(2.1, 2.8, 3.0, 3.2, 3.4, 3.8, 4.0, 4.6, 4.9, 5.0, 5.2, 6.0),
empresaC = c(1.8, 3.5, 4.5, 4.5, 5.0, 4.8, 6.0, 8.0, 8.2, 8.2, 8.5, 9.0))

ganancias2<- data.frame(media= c(mean(ganancias$empresaA),mean(ganancias$empresaB),mean(ganancias$empresaC)),
desviaciontipica= c(sd(ganancias$empresaA),sd(ganancias$empresaB),sd(ganancias$empresaC))); ganancias2

#¿qué significa obtener 4 millones?¿es mucho o poco? DEPENDE de la empresa
#La empresa A es lo que menos ha ganado en estos años, 
#pero es la media de la empresaB (tienen diferente centro)
#¿que significa obtener 8 millones?¿es mucho o poco? DEPENDE de la referencia
#La empresa A es lo que más ha ganado en estos años, 
#mientras la C tiene 4 años de mayores ganancias (tienen diferente dispersión)

#PUNTUACIONES TIPICAS
library(dplyr)
A<-mutate(ganancias, zscore = (ganancias$empresaA-mean(empresaA))/sd(empresaA)) # zscore, tal como está definida aquí, es una variable que normaliza los datos 
                                                                              # de un vector (por ejemplo, una columna de un data.frame).
A; (min(ganancias$empresaA)-mean(ganancias$empresaA))/sd(ganancias$empresaA)#COMPROBACION. Esta operación tiene que dar la zscore del valor mínimo de la columna sobre la que hemos operado.
B<-mutate(ganancias,zscore= (ganancias$empresaB-mean(empresaB))/sd(empresaB))
C<-mutate(ganancias, zscore= (ganancias$empresaC-mean(empresaC))/sd(empresaC))

# Los valores de zscore nos dicen cuántas veces un valor (medido en desviaciones típicas), está por debajo de la media. Por ejemplo: 
# 3 desviaciones típicas por debajo de la media.

cbind(A,B[,4],C[,4])

#¿que significa obtener 4 millones?
#para la empresa A 4 milloens está 1,73 desviaciones típicas por debajo de su media 
#para la empresa B, es la media 
#¿que significa obtener 8 millones?¿es mucho o poco? DEPENDE de la referencia
#tanto para la empresa A como para la empresa C, 8 millones se alejan 2 millones de su media
#pero en términos relativos, ganar 8 millones para la empresa A
#es el doble (1,73) que para la empresa C (0,86).

#¿que supondria una ganancia equivalente en las empresas A y B?
not <-encuesta$Nota_PAU
hist(not,
     xlab   = "Nota",
     ylab   = "Frecuencia",
     main   = "Nota en la PAU",
     col    = "dodgerblue",
     border = "darkorange")


#*********************************Tests estadísticos