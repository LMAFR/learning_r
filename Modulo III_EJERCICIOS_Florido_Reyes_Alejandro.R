################################MODULO III EJERCICIOS

setwd("C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/D�a 5 - Curvas de ajuste")

#Ej.1. Importa la base de datos "agregados.csv" y averigua que variables categoricas contiene

agreg <- read.csv("agregados.csv", header = T); str(agreg)

# No hay factores como tal, pero podr�amos considerar que la variable CCAA es categ�rica, ya que sus valores son un conjunto cerrado de 
# grupos en forma de strings. Yendo m�s all�, podr�amos incluso considerar que las fechas tambi�n lo son, ya que tambi�n vienen dadas en
# formato string, de modo que no puedes obtener muchos estad�sticos t�picos de ella de forma directa (media, dispersi�n, ...) y cada d�a 
# se podr�a considerar un grupo o categor�a.


### Muestra las frecuencias de la variable CCAA en una tabla 

table(agreg$CCAA) # Aparecen una serie de mensajes indeseables que deben estar en alg�n punto de la tabla. Vamos a eliminarlos.

tail(agreg$CCAA, 9) # Tanteando con tail, nos damos cuenta de que hemos metido en el an�lisis los mensajes incluidos de forma adicional a la
                    # tabla al final del documento agregados.csv. Vamos a quitarlos.

agreg <- head(agreg, -10) # Sobreescribimos la variable agreg quitando los mensajes (ten cuidado de no ejecutar el comando varias veces para no perder datos).
tail(agreg$CCAA, 9)       # Comprobaci�n.

table(agreg$CCAA) # Ahora la tabla de frecuencias se ve m�s claramente.

### Muestra las frecuencias de la variable CCAA en un grafico apropiado para este tipo de datos

# Opci�n f�cil:

tf <- table(agreg$CCAA)
barplot(tf, 
        main="Frequencies of each CCAA",
        xlab="CCAA",
        ylab="Frequency",
        border="red",
        col="grey",
        density=100) # Puede que a priori no veas todos los nombres de las abscisas, pero para hacerlo solo tienes que pulsar zoom en la 
                     # ventana del gr�fico o agrandarlo.

# Con ggplot2:

library(ggplot2)

ggplot(agreg) + geom_bar(aes(CCAA), color ='red', fill = 'blue', alpha = 0.3) + labs(title = 'Gr�fico de barras', x = "Comunidades aut�nomas", y = "Frecuencia")+ theme_minimal()

# Ej 2. Representa la variable Hospitalizados en un histograma
# y comp�rala con la distribucion normal esperada.

Hosp <- agreg$Hospitalizados[!is.na(agreg$Hospitalizados)] # Al ver la columna Hospitalizados, nos damos cuenta de que contiene NA's, los 
                                                           # quitamos para poder operar sin que el resultado sea NA.

summary(agreg$Hospitalizados)

mean (agreg$Hospitalizados)
mean (Hosp)
sd(Hosp)

hist(agreg$Hospitalizados, freq = F); # A mayor sea el n�mero de hospitalizados en un d�a y CCAA, menor es la frecuencia con la que se repite ese dato. Parece l�gico.
x <- seq(0, max(Hosp), 10)
curve(dnorm(x, mean = mean(Hosp),sd = sd(Hosp)), add = T) # Dibujamos la curva gaussiana en el intervalo en el que se encuentran los datos de Hospitalizados.
                                                          # La curva se afina aumentando la cantidad de valores en el vector x. He dibujado el histograma
                                                          # en forma de densidad de prob porque la gaussiana te devuelve la probabilidad para cada punto, 
                                                          # no la frecuencia (al menos con la funci�n dnorm).

# Tambi�n lo podr�a haber hecho con ggplot, asign�ndole a un smooth la f�rmula de 

ggplot(agreg, aes(x = agreg$Hospitalizados)) +
  geom_histogram(aes(y = ..density..), color = "Red", fill = "Blue2", alpha =0.3) +
  stat_function(fun = dnorm, args = list(mean = mean(Hosp), sd = sd(Hosp)), color = "Black") +
  labs(title = "Histograma de hospitalizados", x = "Hospitalizados", y = "Densidad") +
  theme_minimal()

#Ej. 3. Con la base de datos "encuesta.dat", contesta a las siguientes preguntas:

### Observa las puntuaciones de las variables de Edad y Unidades de alcohol e intenta hacer comparaciones entre ellas
### Ahora fijate en la dispersion de las notas esperadas en la PAU y las notas obtenidas, �que puedes comentar sobre ellas?
### Haz un grafico adecuado que las represente

# Parte 1.

encuesta <- read.table("encuesta.dat", header = T, sep = "\t", dec = ","); encuesta
x <- encuesta$UnidadesAlcohol
y <- encuesta$Edad
df <- data.frame(x,y); df

mean(x); mean(y)  # Las medias se diferencian en un factor 10, ya para empezar.
sd(x); sd(y)      # Adem�s, su dispersi�n es muy diferente. Necesitamos normalizar para poder comparar.

xn <- (x-mean(x))/sd(x) # Normalizamos usando la f�rmula de la zscore.
yn <- (y-mean(y))/sd(y)

df$zscore_x <- xn # Lo a�adimos al data frame que hemos creado con las variables que quer�amos comparar.
df$zscore_y <- yn 

df # Ahora podemos ver las zscores y comparar ambas variables us�ndola.

# Ejemplos de comparaciones: la persona con 47 a�os est� muy por encima de la media de la muestra y, 
# sin embargo, la cantidad de alcohol que consumen seg�n la encuesta est� por debajo de la media. 
# En general, necesitar�amos agrupar por edades para poder hablar del resto de edades, porque la 
# gente con 18 a�os est� por debajo de la media, pero algunos toman m�s alcohol que la media y otros menos.

  # Parte 2.

  sd(encuesta$Nota_PAU)
  sd(encuesta$NotaEsperada)

  # Como las desviaciones estandar son parecidas, eso significa que la dispersi�n de las notas esperadas y de las notas obtenidas, son similares.

  # Si queremos ver la dispersi�n, lo suyo es dibujar un gr�fico de dispersi�n:

  ggplot(encuesta, aes(NotaEsperada, Edad)) + geom_point(colour = "Red") + geom_point(aes(Nota_PAU, Edad), colour = "Blue", shape ="�", size = 3)

  # Como se puede observar, las nubes de puntos est�n mayormente centradas en torno a la misma zona y no hay muchos puntos que se salgan de la 
  # misma (en el azul hay m�s, pero tambi�n hay m�s puntos en total), por lo tanto, podemos suponer que la dispersi�n de ambos conjuntos es muy
  # parecida.

#Ej. 4. Estandariza las puntuaciones de las variables fluidez en espa�ol y fluidez en ingles

### Comenta las puntuaciones estandarizadas obtenidas con respecto a las puntuaciones directas

  # Las variables mencionadas est�n en el df encuesta, vamos a sacarlas y a crear nuestro propio data frame con ellas para que 
  # luego sea m�s f�cil compararlas.

  esp <- encuesta$FluidezVerbalEspanol
  eng <- encuesta$FluidezVerbalIngles
  df2 <- data.frame(esp,eng); df2
  
  mean(esp); mean(eng)  # Las medias se diferencian en un factor 2.5, ya para empezar.
  sd(esp); sd(eng)      # Su dispersi�n no es muy diferente (un factor 1.5 de diferencia aprox.). En todo caso, necesitamos normalizar
                        # para poder comparar.
  
  espn <- (esp-mean(esp))/sd(esp) # Normalizamos usando la f�rmula de la zscore.
  engn <- (eng-mean(eng))/sd(eng)
  
  df2$esp_norm <- espn # Lo a�adimos al data frame que hemos creado con las variables que quer�amos comparar.
  df2$eng_norm <- engn 
  
  df2 # Ahora podemos ver las zscores y comparar ambas variables us�ndolas.

  max(df2$esp_norm); max(df2$eng_norm)
  
  # Entre otras cosas, en las puntuaciones estandarizadas podemos ver que para el espa�ol la m�xima diferencia con la media se obtiene para un 
  # 18, siendo la desviaci�n de 2.52 sd's, mientras que para el ingl�s es de 2,66  para un 10. A su vez, para desviaciones peque�as (valores 
  # cercanos a la media) tenemos un 11 en espa�ol y un 4 en ingl�s, de modo que hay mucha diferencia de partida en el nivel de fluidez para ambos
  # idiomas. Por �ltimo, el valor m�s bajo de fluidez en espa�ol se tiene para una desviaci�n de -2.07 y corresponde a un 4, mientras que en ingl�s
  # se obtiene para -1.39, que corresponde a un 1.
  
  
    