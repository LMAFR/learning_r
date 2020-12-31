#Ejercicios MODULO III Estadística con R (II)

#1. Construye la distribución muestral del estadístico Media 
#para una población de 5 valores: Yi = {1, 2, 3, 4, 5}
#en muestras de tamaño n=2

  # Como tenemos 5 valores y queremos todas las muestras posibles de 2 valores, obtenemos 5^2 = 25 muestras.
  x <- sample(1:5, size = 25, replace = T); x
  y <- sample(1:5, size = 25, replace = T); y
  mea <- 1:25
  for (i in c(1:25)){mea[i] = mean(c(x[i],y[i]))};mea
  df <- data.frame(x,y,mea);df
  
  # Pero esto no está bien, porque no estamos contando con la probabilidad acumulada que se daría si generáramos las muestras por separado.
  # En el caso anterior cada vez que generamos una pareja, sus dos valores han tenido la misma probabilidad de aparecer, si se generan por parejas eso no ocurre.
  
  # Vamos a hacerlo correctamente:
  
  medias = c()
  for (i in c(1:25)){
    medias = c(medias,mean(sample(valores, 2, replace = T)))
  }; medias
  
    #2. Representa el resultado en un histograma
  
  ggplot(df,aes(x=medias)) +
    geom_histogram(aes(y =..density..)) +
    stat_function(fun = dnorm, args= list(mean(medias),sd(medias)), color = "Black") # No sé por qué las representa separadas, pero bueno.
  
  hist(medias)

sd(df$mea)
    