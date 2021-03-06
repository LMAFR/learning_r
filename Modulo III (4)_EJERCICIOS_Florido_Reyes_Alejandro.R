#Ejercicios Modulo III (4)

setwd("C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/D�a 8")

#Ej 1. Comprueba los supuestos del ANOVA de un factor hecho en clase: 
#*Vamos a ver si existen diferencias en el gasto de las comunidades de 
#*Andalucia, Baleares y Canarias desde el 2004 hasta el 2017
library(readxl)
gasto <- read_excel('Gasto_de_los_turistas_segun_destino_principal.xlsx',
                    col_names = TRUE,skip=1)

#* 1)Hip�tesis de aleatoriedad e independencia: suponemos que las muestras han sido elegidas de 
#* forma aleatoria y que los datos tomados son independientes entre s�, ya que no hemos visto
#* tests para comprobarlo.

#1.1. HOMOCEDASTICIDAD
?bartlett.test

#* 2) Hip�tesis de varianzas iguales (homocedasticidad). Al igual que ocurr�a con las medias, no
#* podemos comparar las varianzas dos a dos con var.test, as� que vamos a usar un test especial.
 
variablegrupo<-rep(c("AN","IB","CA"),each=14) #* Creamos un vector con las CCAA que vamos a tratar
                                              #* replicadas para que ha cada gasto le asociemos 
                                              #* la comunidad correspondiente.

gastoAN<- as.data.frame(t(gasto[1,4:17]))
gastoIB<- as.data.frame(t(gasto[2,4:17])) 
gastoCA<- as.data.frame(t(gasto[3,4:17])) 
gastos<- c(gastoAN$V1,gastoIB$V1,gastoCA$V1)

bartlett.test(gastos, variablegrupo)
bartlett.test(gastos ~ variablegrupo) # Da lo mismo.
 
#* p =0.2665>0.05 = alfa, luego mantenemos la hip�tesis y no hace falta tener en cuenta
#* detalles como el de Bonferroni.

#1.2. NORMALIDAD

#* 3) Hip�tesis de normalidad. Para el test t de Student vimos que hab�a varias posibilidades, pero
#* para empezar vamos a representar gr�ficamente cada poblaci�n, ya que no sabemos si realmente son
#* v�lidas esas pruebas adicionales que vimos en el caso del test t, en el caso del ANOVA (al haber
#* m�s categor�as).

library(lattice)

gastos <- as.data.frame(gastos)
gastos$CCAA <- as.factor(variablegrupo)
gastos

histogram(~ gastos$gastos | gastos$CCAA,
          layout=c(1,3)) 

# Aqu� he dibujado los tres gr�ficos a la vez en una sola ventana, ya se observa una notable
# asimetr�a en cada histograma. Ve�moslos por separado:

ggplot(gastoAN) +
  geom_histogram(aes(V1), color = "cyan", fill = "black", binwidth = 200 ) 
ggplot(gastoCA) +
  geom_histogram(aes(V1), color = "cyan", fill = "black", binwidth = 200 ) 
ggplot(gastoIB) +
  geom_histogram(aes(V1), color = "cyan", fill = "black", binwidth = 100 ) 

#* Por lo que vemos a primera vista en los histogramas, est� claro que los datos no siguen una 
#* distribuci�n normal (ninguna es sim�trica), por lo que con esta hip�tesis podemos descartar
#* la realizaci�n del ANOVA que hicimos en clase el 30/11, ya que sus resultados no deber�an 
#* ser significativos.

# Otro gr�fico que puede ser de ayuda en estos casos es el de caja y bigotes:

boxplot(gastos$gastos~gastos$CCAA)

# Se puede observar que las medianas son bastante diferentes unas de otras y que, incluso,
# los valores extremos para AN est�n contenidos dentro de los valores t�picos de CA, esto es,
# hay bastante diferencia entre los rangos de valores t�picos. Dicho eso, puesto que el n� de
# valores at�picos es el mismo para todos las CA y dichos valores parecen estar m�s o menos a
# la misma distancia de la mediana, podemos intuir que la separaci�n entre las medias ser� 
# similar parecida a la separaci�n entre las medianas. En este supuesto tambi�n influye el alto
# de los boxplots, pero como tampoco hay ninguno notablemente diferente del resto, esto no debe
# afectar en gran medida a esta suposici�n. En conclusi�n: la interpretaci�n de estos boxplots
# parece indicar directamente que no se cumple la hip�tesis nula. 

# Con respecto a la normalidad,
# si la distribuci�n fuera normal, esperar�amos que el n�mero de valores at�picos fuera parecido
# a ambos lados del boxplot (cosa que no ocurre, proporci�n de 2:0) y que los cuartiles estuvieran
# equiespaciados (todo esto por la simetr�a central de la distribuci�n).

# Tambi�n podemos usar qqnorm para ver si cada muestra se adapta a una distribuci�n normal:

gastosANdf <- gastos[gastos$CCAA == "AN",]; gastosANdf
gastosIBdf <- gastos[gastos$CCAA == "IB",]
gastosCAdf <- gastos[gastos$CCAA == "CA",]

qqnorm(gastosANdf[,1]); qqline(gastosANdf[1],distribution = qnorm)
qqnorm(gastosIBdf[,1]); qqline(gastosIBdf[1],distribution = qnorm)
qqnorm(gastosCAdf[,1]); qqline(gastosCAdf[1],distribution = qnorm)

# Podemos observar que las distribuciones tienen un tramo donde se adaptan bien a la l�nea,
# pero despu�s se alejan mucho del comportamiento lineal, de modo que las distribuciones no
# ser�n normales (como ya hemos deducido tambi�n con otros gr�ficos), sino de otro tipo.

# En cuanto a estad�sticos, con describe podemos ver los valores de kurtosis y simetr�a de las 
# distribuciones:

library(psych)

by(gastos$gastos, gastos$CCAA, describe)

# De este an�lisis concluidos que las kurtosis son adecuadas para una distribuci�n normal, ya
# que est�n en torno a 0.9 y, por lo tanto, entre -7 y 7. Por otro lado, el factor de simetr�a
# (skew) est� en torno a 1.45, por lo que tambi�n est� dentro del intervalo de una buena simetr�a
# (que designamos en clase entre -2 y 2). Como este an�lisis no se corresponde con los dem�s (ya
# que est� indicando que las distribuciones son normales), creo que en este caso los intervalos
# tomados como criterio para designar una buena kurtosis y simetr�a no son v�lidos en este caso.
# COTEJAR EN CLASE.

# Por �ltimo, podemos usar tambi�n los test de Shapiro-Wilk y Kolmogorov-Smirnov para ver si las
# distribuciones son normales:

shapiro.test(gastosANdf$gastos)
shapiro.test(gastosIBdf$gastos)
shapiro.test(gastosCAdf$gastos)

# Shapiro indica que en los tres casos hay que descartar la hip�tesis de que las distribuciones
# son normales, ya que los valores de p obtenidos del an�lisis son inferiores a 0.05 para las
# tres muestras.

ks.test(gastosANdf$gastos, "pnorm")
ks.test(gastosIBdf$gastos, "pnorm")
ks.test(gastosCAdf$gastos, "pnorm")

# El test de K-S coincide con el de Shapiro ya que, de nuevo, los valores de p son inferiores
# a 0.05 para las tres muestras (de hecho, en este caso son valores muy cercanos a cero).

# Duda: In�s no us� directamente el df para llamar a los vectores cuando hizo estos
# an�lisis, sino que primero los guard� en una variable aparte del df (PREGUNTAR POR QU�).

#Ej 2. Haz un analisis estadistico que pruebe si la diferencia de hospitalizados es significativa
##entre las comunidades autonomas de Madrid (CM) Extremadura (EX) y Andalucia (AN)
##en la base "agregados"

#* Entiendo que si se nos ha pedido justificar las hip�tesis del ANOVA para el caso estudiado en 
#* clase, es porque aqu� no se pretende que volvamos a hacer dichos calculos. Partiendo de esa 
#* idea, vamos a crear una nueva variable con las nuevas categor�as y a realizar el an�lisis. 

#* Cargamos la base de datos:

agre <- read.csv('agregados.csv',header = T)

#* Necesitamos un vector con la muestra y otro vector (o factor) con las categor�as, esto lo 
#* podemos conseguir a�adiendo la correspondiente restricci�n a la base de datos original:

CCAA <- agre$CCAA[(agre$CCAA == "CM" | agre$CCAA == "EX" | agre$CCAA == "AN") & !is.na(agre$Hospitalizados)]
Hosp <- as.data.frame(CCAA)
Hosp$muestra <- agre$Hospitalizados[(agre$CCAA == "CM" | agre$CCAA == "EX" | agre$CCAA == "AN") & !is.na(agre$Hospitalizados)]
Hosp

#* Donde hemos tenido en cuenta que hab�a valores NA en la columna de hospitalizados (lo vi al correr
#* las l�neas anteriores sin la condici�n de los NAs) y los he quitado para que no exista la posibilidad
#* de que modifiquen los resultados del ANOVA.

anova(lm(Hosp$muestra ~ Hosp$CCAA))

#* Tras obtener los resultados, parece ser que la presencia de los NAs no estaba enturbiando los
#* resultados (se obtiene el mismo valor de p).
#* 
#* Dicho eso, p es pr�cticamente 0 y, por lo tanto, bastante inferior a 0.05 (alfa), de modo que
#* descartamos la hip�tesis nula (media de hospitalizados igual en las tres CCAA).

#2.1. �Podrias representar las medias en un grafico?

# Para que sea m�s visual, vamos a representarlas mediante verticales en un histograma:

library(ggplot2)

str(Hosp)
Hosp$CCAA <- as.factor(Hosp$CCAA)

ggplot(Hosp, aes(x = muestra)) + 
  geom_histogram(aes(fill=Hosp$CCAA), binwidth = 200, color = "cyan" ) +
  geom_vline(xintercept = mean(Hosp$muestra[CCAA == "AN"]), color = "darkred")+
  geom_vline(xintercept = mean(Hosp$muestra[CCAA == "EX"]), color = "darkblue")+
  geom_vline(xintercept = mean(Hosp$muestra[CCAA == "CM"]), color = "darkgreen")+
  labs(x = "N�mero de hospitalizados total", y = "Frecuencia", title = "Histograma del n�mero de hospitalizados total con las medias para cada CCAA")


#* Efectivamente, vemos que las medias de las tres comunidades son bastante diferentes,
#* apreci�ndose claramente diferentes picos en cada distribuci�n y
#* destacando la de extremadura por tener una media de hospitalizados especialmente baja.
