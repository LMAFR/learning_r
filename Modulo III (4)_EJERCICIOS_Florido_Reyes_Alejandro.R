#Ejercicios Modulo III (4)

setwd("C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/Día 8")

#Ej 1. Comprueba los supuestos del ANOVA de un factor hecho en clase: 
#*Vamos a ver si existen diferencias en el gasto de las comunidades de 
#*Andalucia, Baleares y Canarias desde el 2004 hasta el 2017
library(readxl)
gasto <- read_excel('Gasto_de_los_turistas_segun_destino_principal.xlsx',
                    col_names = TRUE,skip=1)

#* 1)Hipótesis de aleatoriedad e independencia: suponemos que las muestras han sido elegidas de 
#* forma aleatoria y que los datos tomados son independientes entre sí, ya que no hemos visto
#* tests para comprobarlo.

#1.1. HOMOCEDASTICIDAD
?bartlett.test

#* 2) Hipótesis de varianzas iguales (homocedasticidad). Al igual que ocurría con las medias, no
#* podemos comparar las varianzas dos a dos con var.test, así que vamos a usar un test especial.
 
variablegrupo<-rep(c("AN","IB","CA"),each=14) #* Creamos un vector con las CCAA que vamos a tratar
                                              #* replicadas para que ha cada gasto le asociemos 
                                              #* la comunidad correspondiente.

gastoAN<- as.data.frame(t(gasto[1,4:17]))
gastoIB<- as.data.frame(t(gasto[2,4:17])) 
gastoCA<- as.data.frame(t(gasto[3,4:17])) 
gastos<- c(gastoAN$V1,gastoIB$V1,gastoCA$V1)

bartlett.test(gastos, variablegrupo)
bartlett.test(gastos ~ variablegrupo) # Da lo mismo.
 
#* p =0.2665>0.05 = alfa, luego mantenemos la hipótesis y no hace falta tener en cuenta
#* detalles como el de Bonferroni.

#1.2. NORMALIDAD

#* 3) Hipótesis de normalidad. Para el test t de Student vimos que había varias posibilidades, pero
#* para empezar vamos a representar gráficamente cada población, ya que no sabemos si realmente son
#* válidas esas pruebas adicionales que vimos en el caso del test t, en el caso del ANOVA (al haber
#* más categorías).

library(lattice)

gastos <- as.data.frame(gastos)
gastos$CCAA <- as.factor(variablegrupo)
gastos

histogram(~ gastos$gastos | gastos$CCAA,
          layout=c(1,3)) 

# Aquí he dibujado los tres gráficos a la vez en una sola ventana, ya se observa una notable
# asimetría en cada histograma. Veámoslos por separado:

ggplot(gastoAN) +
  geom_histogram(aes(V1), color = "cyan", fill = "black", binwidth = 200 ) 
ggplot(gastoCA) +
  geom_histogram(aes(V1), color = "cyan", fill = "black", binwidth = 200 ) 
ggplot(gastoIB) +
  geom_histogram(aes(V1), color = "cyan", fill = "black", binwidth = 100 ) 

#* Por lo que vemos a primera vista en los histogramas, está claro que los datos no siguen una 
#* distribución normal (ninguna es simétrica), por lo que con esta hipótesis podemos descartar
#* la realización del ANOVA que hicimos en clase el 30/11, ya que sus resultados no deberían 
#* ser significativos.

# Otro gráfico que puede ser de ayuda en estos casos es el de caja y bigotes:

boxplot(gastos$gastos~gastos$CCAA)

# Se puede observar que las medianas son bastante diferentes unas de otras y que, incluso,
# los valores extremos para AN están contenidos dentro de los valores típicos de CA, esto es,
# hay bastante diferencia entre los rangos de valores típicos. Dicho eso, puesto que el nº de
# valores atípicos es el mismo para todos las CA y dichos valores parecen estar más o menos a
# la misma distancia de la mediana, podemos intuir que la separación entre las medias será 
# similar parecida a la separación entre las medianas. En este supuesto también influye el alto
# de los boxplots, pero como tampoco hay ninguno notablemente diferente del resto, esto no debe
# afectar en gran medida a esta suposición. En conclusión: la interpretación de estos boxplots
# parece indicar directamente que no se cumple la hipótesis nula. 

# Con respecto a la normalidad,
# si la distribución fuera normal, esperaríamos que el número de valores atípicos fuera parecido
# a ambos lados del boxplot (cosa que no ocurre, proporción de 2:0) y que los cuartiles estuvieran
# equiespaciados (todo esto por la simetría central de la distribución).

# También podemos usar qqnorm para ver si cada muestra se adapta a una distribución normal:

gastosANdf <- gastos[gastos$CCAA == "AN",]; gastosANdf
gastosIBdf <- gastos[gastos$CCAA == "IB",]
gastosCAdf <- gastos[gastos$CCAA == "CA",]

qqnorm(gastosANdf[,1]); qqline(gastosANdf[1],distribution = qnorm)
qqnorm(gastosIBdf[,1]); qqline(gastosIBdf[1],distribution = qnorm)
qqnorm(gastosCAdf[,1]); qqline(gastosCAdf[1],distribution = qnorm)

# Podemos observar que las distribuciones tienen un tramo donde se adaptan bien a la línea,
# pero después se alejan mucho del comportamiento lineal, de modo que las distribuciones no
# serán normales (como ya hemos deducido también con otros gráficos), sino de otro tipo.

# En cuanto a estadísticos, con describe podemos ver los valores de kurtosis y simetría de las 
# distribuciones:

library(psych)

by(gastos$gastos, gastos$CCAA, describe)

# De este análisis concluidos que las kurtosis son adecuadas para una distribución normal, ya
# que están en torno a 0.9 y, por lo tanto, entre -7 y 7. Por otro lado, el factor de simetría
# (skew) está en torno a 1.45, por lo que también está dentro del intervalo de una buena simetría
# (que designamos en clase entre -2 y 2). Como este análisis no se corresponde con los demás (ya
# que está indicando que las distribuciones son normales), creo que en este caso los intervalos
# tomados como criterio para designar una buena kurtosis y simetría no son válidos en este caso.
# COTEJAR EN CLASE.

# Por último, podemos usar también los test de Shapiro-Wilk y Kolmogorov-Smirnov para ver si las
# distribuciones son normales:

shapiro.test(gastosANdf$gastos)
shapiro.test(gastosIBdf$gastos)
shapiro.test(gastosCAdf$gastos)

# Shapiro indica que en los tres casos hay que descartar la hipótesis de que las distribuciones
# son normales, ya que los valores de p obtenidos del análisis son inferiores a 0.05 para las
# tres muestras.

ks.test(gastosANdf$gastos, "pnorm")
ks.test(gastosIBdf$gastos, "pnorm")
ks.test(gastosCAdf$gastos, "pnorm")

# El test de K-S coincide con el de Shapiro ya que, de nuevo, los valores de p son inferiores
# a 0.05 para las tres muestras (de hecho, en este caso son valores muy cercanos a cero).

# Duda: Inés no usó directamente el df para llamar a los vectores cuando hizo estos
# análisis, sino que primero los guardó en una variable aparte del df (PREGUNTAR POR QUÉ).

#Ej 2. Haz un analisis estadistico que pruebe si la diferencia de hospitalizados es significativa
##entre las comunidades autonomas de Madrid (CM) Extremadura (EX) y Andalucia (AN)
##en la base "agregados"

#* Entiendo que si se nos ha pedido justificar las hipótesis del ANOVA para el caso estudiado en 
#* clase, es porque aquí no se pretende que volvamos a hacer dichos calculos. Partiendo de esa 
#* idea, vamos a crear una nueva variable con las nuevas categorías y a realizar el análisis. 

#* Cargamos la base de datos:

agre <- read.csv('agregados.csv',header = T)

#* Necesitamos un vector con la muestra y otro vector (o factor) con las categorías, esto lo 
#* podemos conseguir añadiendo la correspondiente restricción a la base de datos original:

CCAA <- agre$CCAA[(agre$CCAA == "CM" | agre$CCAA == "EX" | agre$CCAA == "AN") & !is.na(agre$Hospitalizados)]
Hosp <- as.data.frame(CCAA)
Hosp$muestra <- agre$Hospitalizados[(agre$CCAA == "CM" | agre$CCAA == "EX" | agre$CCAA == "AN") & !is.na(agre$Hospitalizados)]
Hosp

#* Donde hemos tenido en cuenta que había valores NA en la columna de hospitalizados (lo vi al correr
#* las líneas anteriores sin la condición de los NAs) y los he quitado para que no exista la posibilidad
#* de que modifiquen los resultados del ANOVA.

anova(lm(Hosp$muestra ~ Hosp$CCAA))

#* Tras obtener los resultados, parece ser que la presencia de los NAs no estaba enturbiando los
#* resultados (se obtiene el mismo valor de p).
#* 
#* Dicho eso, p es prácticamente 0 y, por lo tanto, bastante inferior a 0.05 (alfa), de modo que
#* descartamos la hipótesis nula (media de hospitalizados igual en las tres CCAA).

#2.1. ¿Podrias representar las medias en un grafico?

# Para que sea más visual, vamos a representarlas mediante verticales en un histograma:

library(ggplot2)

str(Hosp)
Hosp$CCAA <- as.factor(Hosp$CCAA)

ggplot(Hosp, aes(x = muestra)) + 
  geom_histogram(aes(fill=Hosp$CCAA), binwidth = 200, color = "cyan" ) +
  geom_vline(xintercept = mean(Hosp$muestra[CCAA == "AN"]), color = "darkred")+
  geom_vline(xintercept = mean(Hosp$muestra[CCAA == "EX"]), color = "darkblue")+
  geom_vline(xintercept = mean(Hosp$muestra[CCAA == "CM"]), color = "darkgreen")+
  labs(x = "Número de hospitalizados total", y = "Frecuencia", title = "Histograma del número de hospitalizados total con las medias para cada CCAA")


#* Efectivamente, vemos que las medias de las tres comunidades son bastante diferentes,
#* apreciándose claramente diferentes picos en cada distribución y
#* destacando la de extremadura por tener una media de hospitalizados especialmente baja.
