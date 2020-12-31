#############################MODULO III (2) FUNCIONES DE PROBABILIDAD

#COMO GENERAR LA FUNCION DE PROBABILIDAD DE LA CURVA NORMAL
x=seq(-4,4,length=200) #creamos un vector cualquiera
mean(x);sd(x)

y=1/sqrt(2*pi)*exp(-x^2/2) #calculamos la funci?n de densidad de la curva normal tipificada (media=0, DT=1) a partir de su formula

plot(x,y,type="l",lwd=2,col="red") #representamos los valores de x en base a una funci?n de probabilidad normal tipificada:
#a cada valor de x le va a corresponder una probabilidad segun y


###funcion dnorm
x=seq(-4,4,length=200)
y=dnorm(x,mean=0,sd=1) #calcula la funci?n de densidad de la curva normal, 
#sin necesidad de conocer la formula, tan solo especificando la media DT que queremos
plot(x,y,type="l",lwd=2,col="blue")

#COMO CALCULAR PROBABILIDADES
#*Dijimos que calcular la probabilidad de un valor del eje x equivale a 
#*calcular partes del area bajo la curva
#*la funcion pnorm() calcula esas probabilidades

pnorm(0, mean=0, sd=1)# aqui le estamos pidiendo que calcule 
#el area bajo la curva que esta a la izquierda del 0
#sabiendo que, como dijimos, el area total bajo la curva vale 1
#?cuanto valdr? el area hay hay a la izquierda del 0?

pnorm(1, mean=0, sd=1)# aqui le estamos pidiendo que calcule 
#el area bajo la curva que esta a la izquierda del valor 1

# en terminos de probabilidad ?esto que significa?
# Si escogemos un numero al azar de la distribucion normal estandarizada
#la probabilidad de que ese numero sea igual o menor a uno es 0.8413447 en tanto por 1.

#***********************(representacion del area)
x=seq(-4,4,length=200)
y=dnorm(x)# si no se especifica mean y sd, toma el 0 y 1 por defecto
plot(x,y,type="l", lwd=2, col="blue")

x=seq(-4,1,length=200)
y=dnorm(x)
polygon(c(-4,x,1),c(0,y,0),col="gray")
#***********************

#LA REGLA DE DECISION
#*Para decidir si rechazamos o no nuestra hip?tesis nula, usamos la Regla de decisi?n
#*si p valor < nivel de significaci?n (alpha) --> rechazamos la H0 
#*Normalmente se usa el criterio de p< alpha = 0.05 para rechazar la hipótesis.
#*
#*generamos de nuevo la curva normal:
x=seq(-4,4,length=200)
y=dnorm(x)
plot(x,y,type="l", lwd=2, col="blue")


#*?que significa usar el criterios de 0.05?
#*Significa que queremos saber si mi valor tiene el 95% de probabilidad
#*de caer en el area que le indiquemos de de la curva normal tipificada
#*como la curva normal tipificada tiene probabilidades ya establecidas
#*sabemos que el 95% de su ?rea es la que se enmarca entre -2 y 2
#*desviaciones tipicas alejadas del cento (media=0)

#lo comprobamos:
pnorm(2,mean=0,sd=1)-pnorm(-2,mean=0,sd=1)#sale que el area entre 2 y -2 tiene una probabilidad asociada de 95%
#******(aclaracion)
#*(como hemos dicho, pnorm() nos dara el area (probabilidad) que queda a la izquierda del numero que le indiquemos)
pnorm(2,mean=0,sd=1)# ?que area nos da aqui?
#****************
qnorm(0.98) # Te hace lo contrario que pnorm (por ejemplo, si le metes 0.98, te dará aproximadamente 2).
#**************representamos el area:
x=seq(-2,2,length=200) #vamos a generar una secuencia de numeros que se encuentren 
#2 desviaciones tipicas alejadas de la media 
y=dnorm(x) #con esta funcion generamos la de densidad de la curva normal 
polygon(c(-2,x,2),c(0,y,0), col="gray")#aqui representamos el area en el que caerian esos numeros. 
                                       # Los vectores indican la coordenada x y la coordenada y en cada uno de los
                                       # dos puntos a unir (la x y la y solo indican la coordenada a la que se 
                                       # refiere el vector, no hace nada más y se pueden suprimir).

plot(x,y,type="l", lwd=2, col="blue")

y=dnorm(x) #con esta funcion generamos la de densidad de la curva normal 
polygon(c(-2,x,2),c(0,y,0.2), col="gray")# Ejemplo de un uso random de polygon.


#****************************************
#*
#*Por tanto, cuando la probabilidad de mi valor (que obtenemos en los analisis)
#*es menor al p valor establecido (0.05)
#*nos estar? indicando que cae fuera de ese area 
#*
#*
#*
