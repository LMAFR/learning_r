#Ej. Reproduce los ejemplos de histograma, grafico de barras, boxplot y grafico de dispersion que hemos visto
#en clase, pero esta vez usando el paquete de ggplot2

###1. Instalar y cargar el paquete
??ggplot2
library(ggplot2)


###2. Leer el capitulo 4 "Exploring data with graphs"

###3. Hacer un Histograma con las Notas de la PAU de la base de datos "encuesta"

setwd("C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/Día 4")
encuesta <- read.table("encuesta.dat",header = T,sep = "\t",dec = ","); class(encuesta)

  qplot(encuesta$Nota_PAU, geom = "histogram",
        binwidth=0.5,
        main = "Histograma con las notas de PAU",
        xlab = "Notas",
        ylab = "Frecuencia",
        fill = I("blue"),
        col = I("red"),
        alpha = I(.2), # Transparencia de las columnas.
        xlim = c(4,8)) # L?mites para el eje x. Esto hace que nos devuelva un warning explicando lo que nos ha 
                       # filtrado al meter el intervalo. El warning parece citar tambi?n los NA's.
  ggsave("C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/D?a 4/Graficoqplot.jpeg") # Aprovechamos para guardarlo como jpeg.
  
  # Pero esta funci?n (qplot) no aparece en el cap?tulo del libro, as? que vamos a probar a hacerlo como dice en el libro:
  
  ggplot(encuesta, aes(Nota_PAU)) + geom_histogram(colour = "Cyan", fill = 'Black', binwidth = 1) # As? se dibujar?a el histograma con la funci?n ggplot.
  
  ggsave("C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/D?a 4/Graficoggplot.png")
  
  # Obs?rvese que ggplot permite exportar a varios formatos, entre ellos: jpeg, png, tex (pictex) y pdf.
  
  # El stat que usa para dibujar en el eje Y la frecuencia a falta de una variable es aes(y=..count..), que se escribir?a en el par?ntesis del geom.
  # Otros: aes(y=..density/ncount/ndensity..) para la densidad/frecuencia en tanto por 1/densidad en tanto por uno en lugar de la frecuencia, 
  # binwidth para la anchura de los canales (bins), breaks, etc.
  
 # Me he ayudado tambi?n de este enlace: https://www.datacamp.com/community/tutorials/make-histogram-ggplot2?utm_source=adwords_ppc&utm_campaignid=1655852085&utm_adgroupid=61045433982&utm_device=c&utm_keyword=%2Bggplot%20%2Bhistogram&utm_matchtype=b&utm_network=g&utm_adpostion=&utm_creative=318880582257&utm_targetid=aud-392016246653:kwd-589281897814&utm_loc_interest_ms=&utm_loc_physical_ms=1005493&gclid=CjwKCAiAzNj9BRBDEiwAPsL0d9ySyWJbc2fYtHeOKmL-BjmoxWj9OT5dmLIbh-5jiRPMm6clRYENMRoCbqAQAvD_BwE
  
###4. Hacer un Grafico de barras con la variable Sexo de la base de datos "encuesta"
  
  encuesta$gender <- 0
  encuesta$gender[encuesta$Sexo == 1] <- "Hombre"
  encuesta$gender[encuesta$Sexo == 2] <- "Mujer"
  
  ggplot(encuesta, aes(gender, colour = gender)) + geom_bar(fill ='White') + labs(title = "Mi gr?fico de barras") + theme_bw()
  
  # theme_bw() enmarca la gráfica y cambia el fondo a blanco.

  # Revisar cómo pintar en cada barra hombres por un lado y mujeres por otro. Debe estar en el archivo con las soluciones de la profesora.  
  
###5. Hacer un Diagrama de caja y bigotes con la variable Notas de la PAU de la base de datos "encuesta"
  
  graf5 <- ggplot(encuesta, aes(Sexo, Nota_PAU))
  wb <- graf5 + geom_boxplot() + labs(x = "G?nero", y = "Notas"); wb # No s? por qu? me junta hombres y mujeres en el mismo cuadro, supongo que si no se diferenciaran con valores num?ricos no dar?a este problema.
  
  graf5 <- ggplot(encuesta, aes(gender, Nota_PAU))
  wb <- graf5 + geom_boxplot(aes(colour = gender)) + labs(x = "G?nero", y = "Notas"); wb # Si les pones valores en forma de caracteres a los valores del eje X s? que los separa.
    
  # Se puede usar outlier.shape y otras opciones similares para modificar la apariencia de los outliers.
              
  # He usado aes(colour = gender) para ponerle colores distintos a cada caja y, adem?s, al hacerlo te pinta una leyenda.
  # No s? c?mo meterle el alpha(colour,transparency).
  # No me funciona opts (+ opts(title = "Mi diagrama de caja y bigotes")), aunque puedo poner el t?tulo usando labs.
  
  
  # De nuevo, suponemos que la columna con respecto a la que hab?a que pintar las Notas de PAU se dejaba a nuestra libre elecci?n.
  
###5. Hacer un Grafico de dispersion con la variable Notas de la PAU y Edadde la base de datos "encuesta"
  
  graf6 <- ggplot(encuesta, aes(Edad, Nota_PAU, colour = gender))
  disp <- graf6 + geom_point(shape = "A"); disp # Podemos elegir el s?mbolo usado para cada punto usando shape. Tambi?n hay valores predeterminados si igualamos a los enteros del 1 al 25.
                                                               # Podemos elegir el color usado con colour = "(un color)"
  
  graf7 <- ggplot(encuesta, aes(Edad, Nota_PAU, colour = Sexo))
  disp <- graf7 + geom_point(shape = "A"); disp # Podemos elegir el s?mbolo usado para cada punto usando shape. Tambi?n hay valores predeterminados si igualamos a los enteros del 1 al 25.
  # Podemos elegir el color usado con colour = "(un color)"
  
  graf6 +geom_point()+geom_quantile(colour="Cyan") # Para dibujar los cuartiles.
  
  # Otros detalles que podemos encontrar en los apuntes de la clase siguiente: dubujar smooth (curvas de ajuste)                     