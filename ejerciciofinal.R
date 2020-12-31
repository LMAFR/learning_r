
# ---
#   title: "Ejercicio Final"
# author: "Alejandro Florido Reyes"
# date: "29/11/2020"
# output: html_document
# ---
# En este trabajo hemos realizado un estudio sobre una base de datos de compradores.

# Para poder empezar el estudio, primero tenemos que importar las librerias que vamos a utilizar.

library(dplyr)
library(tidyverse) 
library(ggplot2)
library(lattice) 
library(psych)


## 1) Importar bases de datos en R 

# En primer lugar establecemos el directorio de  trabajo, donde tenemos dos archivo siendo estos:
#   el ejericio en .r y la bbdd. Para poder importar la base de datos con la que vamos a trabajar debemos primero de establecer el directorio de trabajo, lo hacemos de manera manual siguiendo la ruta: *"Pestaña" Session > Set Working Directory > Choose Directory* (y buscamos la carpeta) donde tengamos los ficheros que hemos nombrado anteriormente.

# Invocamos la función para leer el Dataset que queda registrado en la variable compradores.

compradores <- read.csv("Dataset_Compradores.csv", header = TRUE)

head(compradores)

# Usamos el comando *str* para ver la tipología de cada uno de los datos que componen el dataframe. 
# Como vemos abajo, la mayoría de las variables son de tipo int y char.

str(compradores)


# A continuación comprobamos si alguna variable tiene valores nulos en nuestra base de datos compradores. 

is.null(compradores) #no hay ninguna variable con valor null
sum(is.na(compradores)) # no hay ningun valor NA en la base de datos

# Como vemos los resultados son false y 0, lo que significa que no existen valores nulos en la base 
# de datos.

## 2) Describir de manera grafica y estadistica las variables de la base de datos
# Para este apartado, comenzamos realizando un *summary* con el objetivo de entender cada una de los componentes de la base de datos.
# A partir de este análisis rápido podemos empezar a extraer algunas conclusiones:
#   
# * Conocemos que los clientes tienen un rango de edad desde los [18-92] años
# * El máximo de productos adquiridos en una compra es 4.
# * El salario medio estimado de los clientes es 100090,24€.

summary(compradores)


# A continuación hemos convertido las variables categóricas en factores para poder realizar mejor el 
# estudio de los datos. Realizamos este paso porque al importar la base de datos, el programa R las 
# almacena como char y no interpreta que sean variables que tomen un conjunto de datos limitado.

# Convertimos a factor las siguientes variables:
  
compradores$Geography <- as.factor(compradores$Geography)
compradores$Gender <-  as.factor(compradores$Gender)
compradores$HasCrCard <- as.factor(compradores$HasCrCard)
compradores$IsActiveMember <- as.factor(compradores$IsActiveMember)
compradores$Exited <- as.factor(compradores$Exited)


# Tras conocer las variables con el *summary* realizamos el estudio de ellas a través de gráficos.

# Primero queremos conocer a los compradores, para ello tenemos que saber como se distribuyen los 
# clientes por país y si hay diferencias significativas en el género y la edad.

ggplot(compradores) + geom_bar(aes(x=Geography),colour="#DC3E40") + 
  labs(x= "Paises", y= "Frecuencia", title = "Clientes por Paises") +theme_minimal()



# Vemos que los compradores se distributen en tres paises diferentes : Alemania, Francia y España.

# Representamos a los compradores en función del género y país. 

ggplot(compradores, color = "Gender") +
  geom_bar(aes(x = Geography, fill = Gender), color = "Black", position = "dodge") + 
  labs(x= "Paises", y= "Frecuencia", title = "Clientes por Paises dividido por género")+ theme_minimal()


# La primera conclusión que podemos extraer y que se ve a simple vista, es que hay más hombres que 
# mujeres en todos los países.

# A continuación nos enfocamos en conocer la cantidad de personas en función del género.

table(compradores$Gender)

# Como se ve en el resultado, la diferencia entre ambos sexos es casi de mil sujetos. 

((5457-4543)/(4543)*100)
# Hay un 20% más de hombres que de mujeres. 

# Una vez calculado este excedente de hombres, hemos representado la variable género sin segmentar por 
# paises. 

ggplot(compradores) + 
  geom_bar(aes(x=Gender),colour="#DC3E40") +
  labs(x= "genero", y= "Frecuencia", title = "Genero de los clientes") +
  theme_minimal()

# No podemos observar ninguna diferencia apreciable entre el número de hombres y mujeres que compran 
# bienes o servicios en esta empresa, por tanto,  el negocio no está enfocado a suplir las necesidades
# de compra en función de un género.

# A continuación representamos a los compradores en función de su edad y género. 

ggplot(compradores, colour=Gender) + 
  geom_histogram(aes(x=Age, y=..density.., fill=Gender), colour="White", position = "dodge") +
  stat_function(fun=dnorm, args = list(mean=mean(compradores$Age), sd=(sd(compradores$Age)))) +
  labs(x= "Edades", y= "Frecuencia", title = "Frecuencia de las edades de los clientes") +
  theme_minimal()


# La frecuencia de las edades de los clientes sigue una distribución normal con sesgo positivo a la 
# derecha.

# Vamos a representar la misma varible pero con una gráfica de caja y bigotes con el objetivo de estudiar
# los valores atípicos o extremos.

ggplot(compradores) +
  geom_boxplot(aes(x=Age)) + coord_flip() + theme_minimal()


# A simple vista podemos concluir que existen valores atípicos, pero al tener una frecuencia continua 
# los consideramos valores normales exceptuando los dos últimos puntos, los cuales serían los valores 
# atípicos.


# Posteriormente hemos generado una gráfica representando la distribución del número de productos por 
# clientes, para así conocer mejor la cantidad de productos que los consumidores adquieren por compra.


ggplot(compradores, aes(x=NumOfProducts))+geom_bar (colour="#DC3E40")+labs(x= "Número de productos", 
  y= "Frecuencia", title = "Número de productos adquiridos por los clientes") + theme_minimal()


# Gracias a esta gráfica podemos observar que existen "dos grupos" de clientes, ya que tenemos una 
# concentración de los mismos entorno a la adquisición de uno o dos productos y por otro lado, vemos
# como un número muy reducido de clientes adquieren tres o cuatro productos. 

# Para poder visualizar las relaciones más interesantes hemos decidido hacer un plot de todo el dataframe;
# pero como tiene una gran cantidad de varibles, es muy difícil poder establecer las relaciones. 
# A este problema se le suma que muchas de las variables son cuantitativas categóricas por lo que 
# seguimos sin poder establecer una relación clara. 

plot(compradores)

# Tras analizar las gráficas que obtuvimos con *plot* hemos decidido representar los datos del salario 
# estimado y la edad en un boxplot para estudiar la dispersión.

dispersion <-ggplot(compradores, aes(EstimatedSalary, Age, colour=Gender)) +
  geom_point(size=0.8)

dispersion


# Hemos determinado diferenciar cada punto gráficado en función del género del cliente. 

# Llegamos a las siguientes conclusiones: 
  
# 1. Aparentemente, el salario no depende de la edad ni del género, tal y como podemos ver en el gráfico. 
# 2. Cada punto que vemos en la gráfica representa a un comprador. Cada uno de estos compradores puede 
#   tener como mucho 4 productos comprados. Como hemos visto antes, el número de compradores que adquiere 
#   tres o cuatro productos es despreciable, por lo que podemos considerar que la mayor parte de estos 
#   compradores adquiere solo uno o dos productos, además, estos compradores están comprendidos en un 
#   rango de edad de [26-50] años. 
# 3. Por último, postulamos que no existe ninguna tendencia a la hora de comprar en función del género 
#   del cliente.


# A continuación hemos decidido indagar en los balances de los clientes. Hemos interpretado la variable 
# balanza como el dinero que le falta por pagar a cada consumidor, por lo que para poder realizar el 
# estudio, no hemos tenido en cuenta a aquellos clientes cuyo balance es 0, porque ya tienen todo pagado.


Balances <- data.frame(compradores$Balance[compradores$Balance !=0]);

ggplot(compradores) + 
  geom_histogram(aes(x = compradores$EstimatedSalary, y=..density..), color ="White", fill = "Black")  +
  stat_function(fun=dnorm, args = list(mean=mean(compradores$EstimatedSalary), sd=(sd(compradores$EstimatedSalary))), colour= "red") +
  labs(x= "Salario Estimado", y= "Frecuencia", title = "Gráfica en función del salario") +
  theme_minimal() 

ggplot(Balances) + 
  geom_histogram(aes(x = Balances$compradores.Balance.compradores.Balance....0., y=..density..), color ="White", fill = "Black")  +
  stat_function(fun=dnorm, args = list(mean=mean(Balances$compradores.Balance.compradores.Balance....0.), sd=(sd(Balances$compradores.Balance.compradores.Balance....0.))), colour= "red") +
  labs(x= "Balances", y= "Frecuencia", title = "Gráfica en función de balances") +
  theme_minimal() 

# Observamos que, mientras que el salario estimado sigue una distribucción uniforme, el balance sigue
# una distribucción normal. 
# Interpretando "balance" como el dinero que le falta por pagar a cada consumidor, podemos ver que hay
# normalidad, y a pesar de que hay muchos compradores que no tienen deudas, la media entre aquellos que
# todavía tienen gastos pendientes tiene una media de 125.000 euros.

## 3) Análisis estadistico en R 
# Para este ejercicio de analisis se nos ofrecieron dos opciones distintas y al final hemos selecionado:
#   
#   * Realizar un analisis de comparacion, siendo la hipotesis inicial:
#   **el salario de los consumidores es igual en los diferentes paises donde se han recogido los datos** 
#   
  ### Hipótesis iniciales. 
  
  # 1) Las poblaciones se ajustan a una distribución normal o gaussiana:
  
  # *REPRESENTACIONES GRÁFICAS DE LAS MUESTRAS *
  
# El método más habitual para comprobar esta hipótesis es dibujar un histograma para cada muestra y 
# superponer la curva teórica de la distribución normal en caso de ser necesario. 
# En primer lugar, dibujémoslas todas en un mismo gráfico:
  
histogram(~ compradores$EstimatedSalary | compradores$Geography, layout = c(1,3))

# En estos gráficos podemos observar que los datos no se ajustan exactamente a una distribución normal,
# ya que son simétricos pero más que un pico presentan una especie de meseta, sin embargo, esto podría 
# llegar a asociarse con una kurtosis tal que el pico es muy bajo, así que vamos a dibujar cada gráfico
# por separado y a dibujar la curva teórica para cada muestra, de tal forma que podamos sacar conclusiones
# más acertadas.

# Primero creamos tres vectores con los salarios para cada país, aprovechando las funciones de filtrado
# y selección del paquete tidyverse:

ComprEsp <- compradores %>%
  select(EstimatedSalary) %>% 
  filter(compradores$Geography == 'Spain'); summary(ComprEsp)

ComprFr <- compradores %>%
  select(EstimatedSalary) %>% 
  filter(compradores$Geography == 'France'); summary(ComprFr)

ComprGer <- compradores %>%
  select(EstimatedSalary) %>% 
  filter(compradores$Geography == 'Germany'); summary(ComprGer)

# Luego los pasamos a formato dataframe para dibujarlos usando ggplot2 (también incluido en el paquete 
# tidyverse):

dfSp <- data.frame(ComprEsp)
dfFr <- data.frame(ComprFr)
dfGer <- data.frame(ComprGer)


# Finalmente, representamos las muestras por separado junto a sus curvas teóricas:
  
(GrSp <- ggplot(dfSp) +
    geom_histogram(aes(EstimatedSalary, y=..density..), color ="darkblue", fill = "blue") +
    stat_function(fun = dnorm, args = list(mean = mean(dfSp$EstimatedSalary), sd = sd(dfSp$EstimatedSalary)), color = "black"))+
  labs(x= "Estimación de Salario", y= "Frecuencia", title = "Gráfica en función de la estimación de salarios Españoles") +
  theme_minimal()
#ggsave("dfSp.jpeg", GrSp)

(GrFr <- ggplot(dfFr) +
    geom_histogram(aes(EstimatedSalary, y=..density..), color ="darkblue", fill = "blue") +
    stat_function(fun = dnorm, args = list(mean = mean(dfFr$EstimatedSalary), sd = sd(dfFr$EstimatedSalary)), color = "black"))+
  labs(x= "Estimación de Salario", y= "Frecuencia", title = "Gráfica en función de la estimación de salarios Franceses") +
  theme_minimal()
#ggsave("dfFr.jpeg", GrFr)

(GrGer <- ggplot(dfGer) +
    geom_histogram(aes(EstimatedSalary, y=..density..), color ="darkblue", fill = "blue") +
    stat_function(fun = dnorm, args = list(mean = mean(dfGer$EstimatedSalary), sd = sd(dfGer$EstimatedSalary)), color = "black")) +
  labs(x= "Estimación de Salario", y= "Frecuencia", title = "Gráfica en función de la estimación de salarios Alemania") +
  theme_minimal()
#ggsave("dfGer.jpeg", GrGer)

# Luego, ninguna de las tres distribuciones se acaba de parecer a una distribución normal, en líneas 
# generales se parecen más a una uniforme y, por lo tanto, en principio descartaríamos llevar a cabo 
# el ANOVA. De todas formas, como las distribuciones cumplen la propiedad de simetría propia de una 
# distribución normal, vamos a hacer algunos análisis adicionales que nos permitan estar más seguros 
# de nuestra decisión en caso de descartar el ANOVA.

# La función qqplot nos permite saber si una curva se ajusta a una distribución normal comparando una 
# recta teórica con los puntos de la muestra. Veamos que resultado nos devuelve:

qqnorm(dfSp[,1]); qqline(dfSp, distribution = qnorm)
qqnorm(dfFr[,1]); qqline(dfFr, distribution = qnorm)
qqnorm(dfGer[,1]); qqline(dfGer, distribution = qnorm)

# Como se puede observar en las gráficas, las tres son prácticamente iguales (como cabría esperar, ya 
# que sus distribuciones también lo son) y los puntos se adaptan muy bien a la misma durante un gran 
# tramo de la gráfica. No obstante, hay muchos puntos que no solo no se ajustan a la recta, sino que 
# se desvían mucho de la misma. Esto último es un indicio de que las distribuciones no son normales.

# Por último, los boxplots también pueden darnos información sobre cómo de parecida es una distribución 
# a una distribución normal.

boxplot(compradores$EstimatedSalary ~ compradores$Geography)

ggplot(compradores, aes(x=Geography, y=EstimatedSalary, fill=Geography)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=10, color="White", fill="red") +
  theme(legend.position="none") + theme_minimal() +
  scale_fill_brewer(palette="Set1")

# Como podemos observar en los boxplots (a los que hemos añadido la media para ayudar a interpretar los 
# mismos), los cuartiles están equiespaciados entre sí (lo que indica que los valores de los salarios 
# están homogéneamente distribuidos a lo largo de la muestra), esto es una característica tanto de las 
# distribuciones normales como de las uniformes, así que no nos aporta información nueva. Tampoco hay 
# valores atípicos ni extremos, ni hay diferencias notables entre la mediana y la media en ninguna de 
# las muestras, el problema es que estas propiedades siguen siendo propias tanto de una distribución 
# gaussiana como de una distribución uniforme.

# Como no parece que podamos afinar más nuestras conclusiones a través del análisis gráfico, estudiemos 
# ahora las muestras desde un punto de vista más analítico a través de sus estadísticos.

# *USO DE ESTADÍSTICOS PARA COMPROBAR LA HIPÓTESIS*
  
# En este apartado usaremos la función describe, de la librería psych, para analizar los 
# valores de kurtosis y simetría de las distribuciones.

by(compradores$EstimatedSalary, compradores$Geography, describe)

# En los resultados podemos observar que la simetría de las distribuciones es casi pefecta 
# (skew es prácticamente nulo y nuestro rango de valores aceptables de cara a una distribución 
#   normal son -2 < skew < 2). Por otro lado, la kurtosis también entra dentro de los valores 
# propios de una distribución normal (valores en torno a -1.2 entran dentro del intervalo designado,
# que en este caso es -7 < kurtosis < 7). En consecuencia, los estadísticos de las muestras nos 
# están indicando que sus distribuciones pueden ser consideradas como normales.

# Dicho esto y, para acabar, como es de suponer que valores de kurtosis y simetría tan cercanos a cero
# sean también propios de distribuciones normales, vamos a realizar los tests de Kolmogorov - Smirnov
# y de Shapiro - Wilk.

# *PRUEBAS ESPECÍFICAS PARA COMPROBAR LA HIPÓTESIS DE NORMALIDAD *
  
# **Test de Shapiro - Wilk:**

shapiro.test(dfSp$EstimatedSalary) # p < 2.2 * 10^-16
shapiro.test(dfFr[1:5000,1]) # p < 2.2 * 10^-16
shapiro.test(dfGer$EstimatedSalary) # p < 2.2 *10^-16

nrow(dfFr)

# Hay 5014 datos de salarios franceses y el test solo puede realizarse sobre columnas de entre 
# 3 y 5000 datos, así que en ese test he tomado 5000 valores en lugar del total (5014). Puesto 
# que hemos dejado fuera muy pocos valores con respecto al total, esto no debería afectar a los
# resultados del test (y menos teniendo en cuenta lo drásticos que han sido los valores de p).

# El test de Shapiro - Wilk nos está indicando que ninguna de las muestras posee una distribución normal
# (p < 0.05, luego se descarta la hipótesis de normalidad). Veamos qué resultados nos devuelve el test 
# de Kolmogorov - Smirnov.

# **Test de Kolmogorov - Smirnov:**
  
ks.test(dfSp$EstimatedSalary, "pnorm") # p < 2.2 *10^-16
ks.test(dfFr$EstimatedSalary, "pnorm") # p < 2.2 *10^-16
ks.test(dfGer$EstimatedSalary, "pnorm") # p < 2.2 *10^-16

# Al igual que Shapiro, las pruebas de K-S nos indican que hay que rechazar la hipótesis en los tres 
# casos, esto es, ninguna muestra sigue una distribución normal y, por lo tanto, el ANOVA no debería 
# realizarse para comparar estas tres muestras. Ahora bien, sabemos por lo visto en clase que estos 
# tests son extremadamente sensibles y, por lo tanto, era de esperar este resultado tras los análisis
# anteriores.

# Conclusiones de la hipótesis de normalidad: dado que los tests son muy sensibles y que, 
# por lo general, el resto de análisis han indicado que nuestras muestras poseen una distribución híbrida
# entre una uniforme y una normal, podemos realizar el ANOVA a sabiendas de que los resultados no 
# serán 100% fiables. 

# En https://www.cienciadedatos.net/documentos/19_anova se comenta lo que ya se nos dijo en clase, 
# el ANOVA es un test muy robusto y, en consecuencia, se añade que no es necesario realizar otro tipo
# de test en su lugar a menos que haya grandes problemas de simetría (que no los hay) o un tamaño de 
# muestra pequeño (dentro de lo cual no podemos entrar porque no tenemos un baremo para medir qué es 
# pequeño y qué no, pero eso no viene siendo un problema en nuestro análisis).
# Por lo tanto, este argumento respalda también la realización de un ANOVA para estas muestras.

#* 
# 2) Hipótesis de Varianzas iguales:
  
  
# *PREVIAMENTE A LA CLASE DEL ANOVA *

var.test(dfSp$EstimatedSalary, dfFr$EstimatedSalary) #0.992
var.test(dfGer$EstimatedSalary,dfSp$EstimatedSalary) #1.041
var.test(dfGer$EstimatedSalary, dfFr$EstimatedSalary) #1.033 

# Como podemos observar, los cocientes de las varianzas son cercanos a 1, por lo tanto, 
# la hipótesis de varianzas iguales parece acertada. Además, si tenemos en cuenta que para un intervalo
# de confianza del 95% el valor más distante está a 0.13 de distancia de los valores obtenidos, 
# podemos asegurar que las varianzas prácticamente cumplen la hipótesis.

# *TRAS LA CLASE DONDE VIMOS EL ANOVA *
  
# Ahora sabemos que lo anterior no es correcto, ya que al comparar las varianzas 2 a 2 estamos 
# dejando de tener en cuenta algunos supuestos probabilísticos (no es lo mismo que la hipótesis se 
# cumpla 2 a 2, a que 2 la cumplan y, además, la tercera también, ya que existe una relación de la 
# tercera con esas dos que afecta al cálculo de esas varianzas 2 a 2 y no la estamos teniendo en 
# cuenta en ese "cálculo 2 a 2").

# Por lo tanto, tenemos que hacer una prueba que considere las tres variables simultáneamente y 
# eso lo hemos estudiado en clase en forma del test de Bartlett:

bartlett.test(compradores$EstimatedSalary, compradores$Geography)

# Como podemos observar, el valor de p es, aproximadamente, 0.55 > 0.05 = alfa, 
# luego mantenemos la hipótesis de hemocedasticidad (las varianzas de las tres muestras son iguales).

# 3) Hipótesis de aleatoriedad e independencia

# Suponemos que las muestras de trabajo han sido tomadas de forma aleatoria e independiente,
# puesto que no se nos dice nada, pero es una base de datos inventada y, por lo tanto, 
# se asume que así es. En caso de ser una base de datos real habría que ponerse en contacto con
# el creador para comprobar que, efectivamente, esta hipótesis se cumple.

# * ¿Cual es la hipotesis nula y la hipotesis alternativa? 
  
  # La hipótesis nula es que el salario de los consumidores es igual en los diferentes países que 
  # aparecen en el data frame. La hipótesis alternativa es que los salarios de alguno de los países 
  # es diferente a los del resto.

# Estudio realizado con el t test ANTES de conocer el ANOVA

#*A continuación exponemos el trabajo realizado antes de conocer el Anova. Para realizar el estudio de si el
#*salario de los consumidores es igual en los diferentes paises donde se han recogido los datos hemos decidido 
#*realizar un estudio por pareja (Alemania-Francia, España-Francia, España-Alemania).Con el objetivo de extraer
#*un t-student que nos permita aceptar o rechazar las hipotesis que hemos planteado. 
#* 
#Inicialmente calculamos las medias de los salarios para cada país.
by(compradores$EstimatedSalary,compradores$Geography,mean) 


#*Formulamos nuestras hipotesis:
#* 
#*μ1 es la media salarial de los alemanes
#*μ2 es la media salarial de los franceses
#*μ3 es la media salarial de los españoles

germanyfrance <- compradores[compradores$Geography!= "Spain", ]
spainfrance <- compradores[compradores$Geography!= "Germany", ]
spaingermany <- compradores[compradores$Geography!= "France",]

#*Primer estudio: comparación salario medio aleman con el salario medio francés
#*H0: μ1=μ2
#*H1: u1!=μ2

geografia=germanyfrance$Geography
salarios=germanyfrance$EstimatedSalary
t.test(salarios ~ geografia,var.equal=T)

#* La primera variable está definina entorno a la segunda. Obtenemos un valor de t de -0.86135.
#* En nuestro primer estudio, obtenemos un valor de p-value de 0.3891, por lo que al ser un valor
#* superior a un nivel de significación de  0,05 cae en la zona de aceptación, por lo que no rechazamos H0.
#* Por lo que no hay diferencia significativas en los salarios medios de alemanes y franceses 

#Segundo estudio: Comparación salario medio español con el salario medio francés
#H0: μ3=μ2
#H1: μ3!=μ2
geografia=spainfrance$Geography
salarios=spainfrance$EstimatedSalary
t.test(spainfrance$EstimatedSalary ~ spainfrance$Geography,var.equal=T)

#* La primera variable está definina entorno a la segunda. Obtenemos un valor de t de 0.32612.
#* En nuestro segundo estudio, obtenemos un valor de p-value de 0.7443, por lo que al ser un valor
#* superior a un nivel de significación de  0,05 cae en la zona de aceptación, por lo que no rechazamos H0.
#* Por lo que no hay diferencia significativas en los salarios medios de españoles y franceses 


#Estudio 3: Comparación salario medio español con el salrio medio alemán

#H0: μ3=μ1
#H1: u3!=μ1
geografia=spaingermany$Geography
salarios=spaingermany$EstimatedSalary
t.test(spaingermany$EstimatedSalary ~ spaingermany$Geography,var.equal=T)

#* La primera variable está definina entorno a la segunda. Obtenemos un valor de t de 1.0238.
#* En nuestro tercer estudio, obtenemos un valor de p-value de 0.306, por lo que al ser un valor
#* superior a un nivel de significación de 0,05 cae en la zona de aceptación, por lo que no rechazamos H0.
#* Por lo que no hay diferencia significativas en los salarios medios de españoles y alemanes. 


#* Por tanto, podemos postular que no hay diferencia significativa ente los salarios medios
#* de los españoles, de los franceses y de los alemanes.
#* 
#* 
#* Ahora tras conocer el análisis de varianza Anova, sabemos que lo realizado anteriormente está mal. Porque
#* al hacer multiples comparaciones, aumentamos la probabilidad de encontrar que una de ellas sea significativa 
#* (los p valores se exageran). La prueba t es un método que determina si dos poblaciones son estadisticamente
#* diferentes entre sí, mientras que el ANOVA (que es el método que deberíamos utilizar) determina si dos o más
#* poblaciones son estadísticamente diferentes entre sí.
#* Ambos métodos examinan la diferencia en las medias entre los grupos pero las formas en que determinan 
#* la significación estadística son diferentes. 

# * ¿Que analisis es correcto para testar dicha hipotesis? ¿Por qué? 
  
  # Un ANOVA, porque tenemos una variable cuantitativa (salarios) y una categórica (países) 
  # con más de 2 categorías. Con solo dos categorías podríamos haber realizado un test T de student.

# * Mostrar los resultados y representarlos graficamente 

anova(lm(compradores$EstimatedSalary ~ compradores$Geography))

# Podemos comprobar que p (Pr en el output de la función)= 0.5584 > 0.05 = alfa, 
# luego se mantiene la hipótesis nula (las medias salariales de los tres países son iguales).

# *OTRA FUNCIÓN PARA HACER EL ANOVA Y COMPROBAR QUÉ PAREJAS FALLAN*

analisis2 <- aov(EstimatedSalary ~ Geography, data = compradores); summary(analisis2)

# Como se puede observar, los resultados obtenidos son aparentemente los mismos, 
# pero con menos cifras decimales en algunas ocasiones.

# Podemos comprobar que la hipótesis se cumple 2 a 2 usando la prueba de comparación múltiple 
# post hoc de Tukey (o, simplemente, prueba HSD):
#   
# **TukeyHSD(analisis2)**
  
# En realidad, esta prueba es realmente útil cuando la hipótesis no se cumple, 
# ya que ahí es cuando vamos a querer saber cuál de las parejas no cumple la hipótesis nula y 
# nos hace rechazarla, pero hemos creído adecuado introducir esta función porque podría ser útil en 
# otros análisis y es muy fácil de usar.


# **TEST NO PARAMÉTRICO: TEST DE KRUSKAL-WALLIS **
  
# En este apartado vamos a realizar el test de Kruskal - Wallis (no paramétrico) para compararlo
# con el valor obtenido mediante el ANOVA, ya que los resultados que no apoyaban la hipótesis de
# normalidad se traducen en que, en realidad, deberíamos realizar un pruebas no paramétricas, como esta.
# 
# Este test viene dado en R por la función kruskal.test y es la prueba que se emplea cuando
# tu distribución no es normal (los análisis no paramétricos no necesitan que la distribución
# se adapte a una distribución teórica concreta) y las muestras son independientes (cuando están
# relacionadas se emplea la prueba de Friedman). Dicho eso, apliquémoslo:
  
kruskal.test(EstimatedSalary ~ Geography, data = compradores)


# Al igual que el ANOVA, el test de Kruskal - Wallis devuelve una p > 0.05, aunque en este caso
# no es exactamente la misma, como cabría esperar (p =0.569). Sin embargo, este test termina
# de respaldar los resultados de los ANOVAs realizados previamente, lo que significa que, en
# efecto, en este caso podíamos despreciar esa apariencia de distribución uniforme que tenían
# las distribuciones en pos de los demás factores que sí que apoyaban su interpretación como
# un conjunto de distribuciones normales.
# 
# Existe la posibilidad de decir que el valor de p obtenido mediante K-W es bastante parecido
# al obtenido en los ANOVAs, sin embargo, creo que esto podría no ser acertado, ya que 
# dependiendo de los cálculos que se realicen en el test, esta comparación podría tener sentido
# o no y, por lo tanto, si se quisiera hacer dicha comparación lo ideal sería profundizar 
# primero en las expresiones matemáticas que se usan en ambos test.
# 
# Como p>0.05, mantenemos la hipótesis nula, esto es, las medias salariales de los clientes en
# los tres países se pueden considerar iguales con un intervalo de confianza del 95%.

# El hecho de que esta hipótesis (que era nuestra hipótesis nula) se cumpla, implica que no hay 
# diferencias entre las medias salariales, por lo que los clientes que tiene la marca presentan 
# la misma media salarial en todos los países, pero las medias salariales de las personas (en general)
# en esos países no tiene por qué ser la misma. Esto permite interpretar el resultado del test de la 
# siguiente manera: la marca vende productos que son mayormente adquiridos por personas dentro de 
# un cierto rango de salarios (en torno a esa media), esto es, con un poder adquisitivo dentro de un 
# intervalo dado. Esta conclusión vendría también respaldada por la inexistencia de valores atípicos
# o extremos en las distribuciones (véase el boxplot) y por la forma de meseta de las distribuciones 
# de las muestras (simetría central y un número de compradores similar para un determinado intervalo
#                  de salarios).


