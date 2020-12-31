###################MODULO III (3)

#CUIDADO: antes de realizar cualquier analisis hay que comprobar los supuestos de dicho analisis

#***********************************************************Prueba T de Student para muestras independientes
#*
#* Sirve para comparar medias.
setwd("C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/Dia 7")
encuesta <- read.table("encuesta.dat", header=T, sep="\t",dec=',')
str(encuesta)

ingles <- encuesta$FluidezVerbalIngles # Una variable cuantitativa.
genero <- as.factor (encuesta$Sexo) # Y una variable categórica. Solo puede tener dos valores, para más valores haríamos un ANOVA.
by(ingles,genero,mean) # vamos a poner a prueba que estas dos medias son iguales (H0)


t.test(ingles ~ genero,var.equal=T)# la fluidez va a ser definida (~) a partir del genero. var.equal es el test para varianzas iguales (lo cual hay que comprobar antes de ejecutar esta línea, aunque aquí no lo hayamos hecho).


#t es el estad?stico de contraste calculado, 
#p-value es el nivel de significaci?n de mi estadistico
## Regla de decision: si el p-valor de mi estadistico < al criterio establecido (0.05)
## entonces, rechazamos la H0 ( ya que la probabilidad de que mi 
## resultado caiga dentro del area de aceptacion en la distribucion teorica
## es demasiado peque?a )

# Por tanto, si p=0.3766 -> no rechazo H0: medias iguales 
# (= las medias no son significativamente distintas,
# o la media de fluidez en ingles de hombres y mujeres no es significativamente distinta)

#el intervalo de confianza nos mostraria el nivel de confianza
# = rango de valores en el que encontrar el verdadero valor en la poblacion con un 95% de probabilidad

# df son los grados de libertad del test (no nos metemos en esto).

#************************************************************T de Student para muestras relacionadas
#*Vamos a comparar una variable cuantitativa medida dos veces en el tiempo
#*Por ejemplo, la nota esperada en la PAU antes y despu?s de realizar el examen
esperadapre <- encuesta$NotaEsperada
esperadapost <- sample(0:10,81,replace = T)
diferencias <- esperadapost- esperadapre; diferencias #los valores negativos
#nos indicarian que los estudiantes esperaban una nota mas alta en el pre que en 
#el post

t.test(esperadapre, esperadapost, paired=T) # paired = T para indicar que las muestras están relacionadas.
#p valor < 0.05, por tanto ?? HAy diferencias estadísticamente significativas entre las notas esperadas 
# antes y después de realizar el examen.


#***************************************(cont ma?ana...)
#************************************************************ANOVA de un factor
#* También nos habla de las diferencias entre las medias, pero a partir de los valores de las varianzas.
library(readxl)
gasto <- read_excel('Gasto_de_los_turistas_segun_destino_principal.xlsx',col_names = TRUE,skip=1)
str(gasto)

a?o<- gasto$Gasto_2004
CCAA <- gasto[1:2,]


#************************************************************Correlaci?n de PEarson
