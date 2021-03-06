#Ejercicios Modulo IV

#Ej. 1. Con la base de datos encuesta.dat
setwd("C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/D�a 9")

encuesta <- read.table("encuesta.dat", header = T, sep = "\t", dec = ",")
View(encuesta)

#1.1. Haz un an�lisis que ponga a prueba si la fluidez verbal en espa�ol
#puede predecir la fluidez verbal en ingles

lm(encuesta$FluidezVerbalIngles ~ encuesta$FluidezVerbalEspanol)

summary(lm(encuesta$FluidezVerbalIngles ~ encuesta$FluidezVerbalEspanol))

#* p = 0.000245 < 0.05, se rechaza la hip�tesis nula (que la pendiente de la recta sea nula), por
#* lo que se aprecia una relaci�n lineal significativa entre ambas variables.

#* r^2 =0.1575 (multiple R-squared), es decir, se aprecia cierta linealidad en la curva de ajuste
#* y la varianza (esto es, la dispersi�n) es grande (el valor est� bastante alejado de uno, que
#* ser�an residuos con valor nulo y, por lo tanto, dispersi�n nula).

#1.2.Interpreta y representa los resultados

plot(encuesta$FluidezVerbalIngles ~ encuesta$FluidezVerbalEspanol)
abline(lm(encuesta$FluidezVerbalIngles ~ encuesta$FluidezVerbalEspanol))

#1.3. Haz el pronostico de la fluidez verbal en ingl�s de una persona 
#con fluidez verbal en espa�ol = 12

x = lm(encuesta$FluidezVerbalIngles ~ encuesta$FluidezVerbalEspanol)
y = x$coefficients[1]+x$coefficients[2]*12; y

#1.4. Comprueba los supuestos
##LINEALIDAD

# Realmente vale con el summary(lm(...)), pero vamos a hacerlo aqu� como dice en las diapos.
# El diagrama de dispersi�n ya lo tenemos, as� que no tenemos que dibujarlo.
# Para obtener el coef. de correlaci�n lineal o de Pearson cargamos la correspondiente librer�a:

library(corrplot)

cor.test(encuesta$FluidezVerbalIngles, encuesta$FluidezVerbalEspanol)

# En el gr�fico de dispersi�n podemos ver claramente que el coeficiente de correlaci�n debe ser
# cercano a 0, ya que los puntos est�n muy dispersos y no se aprecia claramente una recta. Vemos,
# por tanto, que el valor de 0.3968 del coeficiente que hemos obtenido es coherente con los datos.

##NORMALIDAD

# Empecemos por un test, por ejemplo el de Kolmogorov - Smirnov:

ks.test(encuesta$FluidezVerbalIngles, encuesta$FluidezVerbalEspanol)

# Se obtieen un p-value menor que 0.05, luego los datos no se adaptan a una distribuci�n normal

# No me ha dado tiempo a m�s en clase, pero ha pasado las soluciones.

##HOMOCEDASTICIDAD
##INDEPENDENCIA
