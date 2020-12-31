#Ejercicios Modulo III (3)

setwd("C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/Dia 7")

#Ej. 1. Calcula la probabilidad de
#*1) una variable que se distribuye normalmente es mayor a 3
#*2) una variable que se distribute normalmente con media = 35 y DT = 6 es mayor a 42

    #1) La función que nos da la probabilidad de que se den valores menores o iguales a uno indicado es pnorm.
    #   Si queremos los mayores a un valor, hacemos 1-pnorm:
    1-pnorm(3)
    #2) Para añadir los valores de la media y la desviación típica, simplemente los añadimos en ese orden al argumento:
    1-pnorm(42,mean=25,sd = 6)
#Ej 2.Abre la base de datos 'agregados.csv'
  
    x <- read.csv(file = "agregados.csv", header = T)
    # Este archivo tenía una serie de datos molestos en las últimas 9 filas (comentarios). Los quitamos:
    y <- head(x,-9); tail(y)
    
#2.1 Genera de manera aleatoria la variable categorica TestDico que contenga valores si,no 
# (como si reflejara que el test ha dado positivo o no)
    
    y$TestDico <- sample(c("Si","No"), size = nrow(y), replace = T);y
    
##2.2. haz un analisis estadistico que pruebe si la diferencia en hospitalizados es diferente
## entre aquellos casos que dieron positivo en el test y aquellos que no
    
    # Veamos en primer lugar si las varianzas son iguales en ambos casos:
    
    var(y$Hospitalizados[y$TestDico == "Si"]) # Nos da NA porque hay NA's en la columna, vamos a hacer la operación sobre la columna sin NA's, 
                                              # a sabiendas que el resultado no tendrá en cuenta aquellos valores que no conocemos de la muestra.
    vsi <- var(y$Hospitalizados[!is.na(y$Hospitalizados) & y$TestDico == "Si"])
    vno <- var(y$Hospitalizados[!is.na(y$Hospitalizados) & y$TestDico == "No"])
    
    vsi/vno # Las varianzas de ambas poblaciones son similares.
            # Suponemos que la toma de muestras a respetado el principio de independencia.
            # Habría que comprobar también si ambas muestras se adaptan a una distribución normal.
    
    library(ggplot2)
    
    ggplot(y, aes(x = Hospitalizados[TestDico == "Si"])) + geom_histogram(aes(y = ..density..))
    
    # Tengo que generar un df por cada muestra con los datos de Hospitalizados que les corresponden, si no no me deja pintarlo (no se puede pintar parte de una columna de un df)
    
    HospSi <- data.frame(Si = y$Hospitalizados[!is.na(y$Hospitalizados) & y$TestDico == "Si"])
    HospNo <- data.frame(No =y$Hospitalizados[!is.na(y$Hospitalizados) & y$TestDico == "No"])
    
    ggplot(HospSi, aes(x = Si)) + geom_histogram(aes(y = ..density..)) + stat_function(fun = dnorm, args = list(mean = mean(HospSi$Si), sd = sd(HospSi$Si)), color ="cyan")
    ggplot(HospNo, aes(x = No)) + geom_histogram(aes(y = ..density..)) + stat_function(fun = dnorm, args = list(mean = mean(HospNo$No), sd = sd(HospNo$No)), color ="cyan")
    
    # Luego podemos apreciar que las distribuciones que tenemos no son distribuciones gaussianas (normales), ya que es 
    # fácilmente apreciable que no son distribuciones normales y, por lo tanto, realmente no deberíamos realizarles el 
    # test t de student para examinar lo que se nos pide. Sin embargo, si consideramoscomo las muestras son muy grandes
    # podríamos realizar el test despreciando que no se cumpla la hipótesis de normalidad, veamos los tamaños:
    
    nrow(HospSi); nrow(HospNo)
    
    # El problema es que no tenemos un criterio para decir si es grande o pequeña (eso dependerá del contexto).
    
    # En cualquier caso, como es lo que se quiere comprobar que sabemos hacer en este ejercicio, vamos a hacer el test 
    # y a ver los resultados, aunque sea por curiosidad:
    
    t.test(y$Hospitalizados ~ y$TestDico, var.equal = T)
    
    # Podemos ver que el p-value está por encima de 0.05, luego podemos mantener nuestra hipótesis si realmente fuera 
    # adecuado realizar este test. Vemos que el intervalo de confianza está entre -1010 y 700, aproximadamente, lo que
    # son valores pequeños para los más grandes de las muestras, pero grandes para los más frecuentes, así que no
    # es de extrañar ese intervalo de confianza (si bien no puede haber valores negativos y eso hace que el intervalo)
    # no sea realmente fiable (al menos el que nos da R aquí). Las medias en ambos grupos son relativamente similares,
    # veamos cuánto:
    
    (mean(HospSi$Si) - mean(HospNo$No))/mean(HospSi$Si)*100
    (mean(HospSi$Si) - mean(HospNo$No))/mean(HospNo$No)*100
    
    # Esto es, la diferencia relativa de las medias con respecto al valor medio de cada población es de menos del 4%, 
    # lo que no está nada mal a primera vista (los estándares de calidad no están definidos, podrían ser más restrictivos)
    # y que esto no fuera realmente una diferencia pequeña (es algo subjetivo).
    
    # La t (estadístico que hace referencia a la diferencia entre medias) y los grados de libertad (df), no hemos 
    # visto cómo interpretarlos. 

##2.3. Comprueba los supuestos de normalidad e igualdad de varianzas. Para ello, ver:

?var.test # Realiza un test F sobre dos muestras de poblaciones normales para comparar sus varianzas.
    
    var.test(HospSi$Si,HospNo$No) #* Como se puede observar, hace lo mismo que yo hice en el apartado 
                                  #* anterior al comprobar si las varianzas eran iguales: calcula las
                                  #* varianzas de ambas muestras y luego las divide para comprobar cuánto
                                  #* difieren a través del cociente de ambas.
                                  #* 
                                  #* Como la p que se obtiene de este an�lisis es <0.05 (alfa)
                                  #* rechazamos la hip�tesis de varianzas iguales y, por lo tanto,
                                  #* no deber�amos realizar el test para comprobar la hip�tesis de
                                  #* medias iguales, ya que no se cumplen las hip�tesis que se tienen
                                  #* que cumplir para que el test T student devuelva un resultado
                                  #* fiable.
    
#graficos donde se vea la distribuci?n, graficos de normalidad, 
#indices de la forma de la distribucion (curtosis y simetría, principalmente)...
    
    #* Los gráficos con la distribución y la curva normal asociada ya los he hecho en el apartado 
    #* anterior. Los hice antes porque, en principio, hay que comprobar los supuestos antes de llevar
    #* a cabo el test, según vimos en clase (entiendo que aquí se quería comprobar primero que sabíamos
    #* llevar a cabo el test con R y ya luego si sabíamos comprobar los supuestos).
    
    # Vamos a ver cuál es la curtosis y la simetría de las distribuciones correspondientes a
    # las muestras estudiadas, para ello usamos la función describe() de la librería psych:
    
    library(psych)
    describe(HospSi$Si) # El valor de la curtosis viene dada como "kurtosis".
    describe(HospNo$No) # El parámetro de simetría es skew.
    
    #* Por convenio, hay una simetr�a adecuada si |skew| <2 y la kurtosis es adecuada 
    #* si |kurtosis|<7. (El intervalo viene definido por estos valores absolutos).
    
    # Si entráramos en profundidad en estos parámetros, podríamos usarlos para distinguir si la distribución
    # es o no similar a una distribución normal, pero aún no hemos aprendido a indagar en ellos así que 
    # simplemente los muestro.

#Ej 3. Haz un analisis estadistico que pruebe si la diferencia en hospitalizados es diferente
## en las comunidades autonomas de Madrid (CM) Extremadura (EX) y Andalucia (AN)

    # Este ejercicio no lo hacemos porque al final no hemos llegado a ver el ANOVA en clase (todavía).
    
###3.1. Comprueba el supuesto de normalidad