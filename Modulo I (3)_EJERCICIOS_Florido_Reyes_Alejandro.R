#Ej.1 Importa los datos de la base "agregados", especificando la coma decimal

x <- read.csv("agregados.csv", header = T, dec = ","); x

#Ej.2 A partir de los datos de la base "Gasto_de_los_turistas_segun_destino_principal",
#calcula el gasto medio de cada comunidad autonoma en el año 2004.

library(readxl)

y <- read_excel("Gasto_de_los_turistas_segun_destino_principal.xlsx", skip = 1); y

mean(y$Gasto_2004)

tapply(y$Gasto_2004, y$CCAA, mean) # Esto es lo que quería que usaramos, pero solo por usar la función, esto es, 
                                     # realmente la función no da operaciones porque hay un dato para cada grupo usado (CCAA), 
                                     # no hay que preocuparse por eso.

#Ej.3 Suma el gasto total (todos los años, todas las comunidades)

gasto = 0

  for (i in y[,4:17]) gasto = gasto + sum(i); gasto

  #Profesora: 

# Suma el gasto total de cada comunidad y crea un objeto llamado "gastoporCCAA" con esos datos 

y$Gasto2004_2017 <- rep(0,nrow(y)); y$Gasto2004_2017

gastoporCCAA = data.frame(CCAA = y$CCAA, Gasto2004_2017 = y$Gasto2004_2017); gastoporCCAA

for (j in 1:nrow(y)) (
  for (i in y[j,4:17]) gastoporCCAA[j,2] = gastoporCCAA[j,2] + sum(i)
  ); gastoporCCAA

  # Profesora: ella lo ha resuelto con apply y MARGIN = 1, gasto [,4:17] y FUN = sum.

#¿y el gasto total por año? Crea un objeto llamado "gastoporaño" con esos datos 

gastoporaño = data.frame(CCAA = c(2004:2017), Gasto_anual = rep(0,14)); gastoporaño

for (j in 4:17) (
  for (i in y[1:nrow(y),j]) gastoporaño[(j-3),2] = gastoporaño[(j-3),2] + sum(i)
); gastoporaño

# Profesora: ella lo ha resuelto con apply y MARGIN = 2, gasto [,4:17] y FUN = sum.

#Ej.4 En este ejercicio tendréis que usar los operadores logicos
#  -¿es el gasto por CCAA menor a 100000?
  
  gastoporCCAA$Gasto2004_2017 < 100000
  
  #Otra opción (que devuelve las posiciones en la tabla):
  
  which(gastoporCCAA$Gasto2004_2017 < 100000)

#   -¿en que comunidades autonomas?
  
  gastoporCCAA$CCAA[gastoporCCAA$Gasto2004_2017 < 100000]
  
#   -¿cuanto habria gastado cada una de ellas?
  
  gastoporCCAA$Gasto2004_2017[gastoporCCAA$Gasto2004_2017 < 100000]
  
#   -¿en que comunidades autonomas el gasto ha sido de entre 100000 y 120000?

  gastoporCCAA$CCAA[100000 <= gastoporCCAA$Gasto2004_2017 & gastoporCCAA$Gasto2004_2017 <= 120000]
  
  #   -¿cuanto habria gastado cada una de ellas?

  gastoporCCAA$Gasto2004_2017[100000 < gastoporCCAA$Gasto2004_2017 & gastoporCCAA$Gasto2004_2017< 120000]
  
# Ej.5 De que clase es el objeto que has creado con los datos de gasto
#   -Quédate con los datos de la comunidad autonoma de Andalucia

  class(gastoporCCAA)
  class(gastoporaño)
  class(gasto)
  
# Ej.6 Averigua si en alguna comunidad autonoma el gasto del 2006 ha sido igual que el gasto en 2007
  
  w = data.frame(CCAA = y$CCAA, rep(T,nrow(y))); w
  
  for (i in 1:nrow(y)) w[i,2] = (y[i,6] == y[i,7]); w
  
#   a) Encuenta el gasto maximo en el 2006
  
  max(y$Gasto_2006)
  
#   b) ¿cual ha sido el gasto minimo en el 2006 y 2007?

  min(y$Gasto_2006,y$Gasto_2007)
  
  min(y$Gasto_2006) # Para comprobar que ha hecho bien la operación, vemos los resultados para cada columna.
  min(y$Gasto_2007)
  
  
# Ej.7 Usando rep() y/o seq(), crea los siguientes vectores
#       - 0 0 0 0 0 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4
#       - 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5 1 2 3 4 5
#       - 1 2 3 4 5 2 3 4 5 6 3 4 5 6 7 4 5 6 7 8 5 6 7 8 9

  rep(seq(0,4),each = 5)
  rep(seq(1,5),times = 5)
  seq(1,5) + rep(seq(0,4), each = length(seq(1,5))) 
    
# Ej.8 Usa la función `paste()` para generar los codigos de un archivo con los 
# resultados de un cuestionario dividido en tres partes (A, B y C), cada una con 8 preguntas.

  v1<-c(rep("A",8),rep("B",8),rep("C",8)); v2<-(1:8); respuesta <- sample(c("Yes", "No"), 24, replace = TRUE, prob = c(0.3, 0.7))

  codigos <- paste((paste(v1,v2, sep = ".")),respuesta, sep = ':');codigos
  
  # Profesora: ha añadido el tercer valor ("respuesta") ya en la siguiente pregunta y ha hecho tres pastes, de manera que no tuviera 
  # que crear los vectores antes de ejecutar el paste.
  
# Inventa las puntuaciones obtenidas por un alumno en el cuestionario y crea un data.frame 
# donde se indique la puntuacion en cada pregunta
  
  alumno <- sample(c("+1", "0"), 24, replace = TRUE, prob = c(0.6, 0.4))
  
  data.frame(Respuestas_correctas = codigos, Puntuacion_alumno = alumno)

#Ej.9 Crea la logica de una respuesta Si o No a una pregunta, teniendo el no probabilidad de 0.9
  
  respuesta <- sample(c("Yes", "No"), 100, replace = TRUE, prob = c(0.1, 0.9)); respuesta

# Comprueba que el no ha salido alrededor de 90 veces de 100 y el si alrededor de 10 veces de 100
  
  sum(respuesta == "No")
  
  # Profesora: ella ha usado table(respuesta):
  
  table(respuesta)

# Ej.10 Crea un objeto que recoja los gastos en el 2008 divididos por comunidades.
  
  data.frame(CCAA = y$CCAA, Gastos_2008 = y$Gasto_2008)
  
  # Otra opción: con select: 
  
  library(dplyr)
  
  select(y, CCAA, Gasto_2008)
  