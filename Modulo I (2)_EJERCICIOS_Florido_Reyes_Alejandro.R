# Ej.1 Instalar el paquete ggplot2 con codigo
install.packages("ggplot2")

# Ej.2 Explorar el paquete dplyr con el siguiente codigo. ¿Que nos permite hacer cada una de sus funciones?
library("dplyr")
?select # Selecciona (y puede renombrar si se quiere) variables en un data.frame, tanto mediante índices como mediante funciones del tipo is.numeric.

?filter # Se usa para filtrar datos de un data.frame a través de condiciones. Si la condición devuelve TRUE para un dato, dicho dato pasa el filtro.
        # Si se encuentra un NA en una de las filas que pasan el filtro, elimina dicha fila, así que cuidado con eso, que no tiene en cuenta dichas filas y puede que 
        # el NA esté en una columna que, a priori, nos daba igual (es decir, estaríamos perdiendo información).

?arrange # Ordena las filas de un data.frame por sus valores en las columnas seleccionadas. A menos que se indiquen explícitamente (o se añada 
         # .by_group = TRUE), este comando ignora las variables agrupadas (caso excepcional). Además, las funciones de variables son evaluadas 
         # una vez por data.frame, no una vez por grupo.

?mutate  # Este comando añade nuevas variables y preserva las existentes, pero si dicha variable ya existe la sobreescribe, de modo que el comando sirve 
         # para añadir nuevas variables a un conjunto y para modificar las ya existentes. Existe también otro comando (transmute) que crea nuevas variables
         # a costa de eliminar otras (dicho comando siempre elimina algo).

?summarise # Crea un nuevo data.frame con una o más filas para cada variable o combinación de variables agrupadas. Si no hay variables agrupadas, el 
           # resultado será una única fila que resuma todas las observaciones sobre el data.frame. Por otro lado, habrá una columna para cada variable 
           # agrupada y cada valor estadístico que hayamos específicado en el argumento de la función.

?group_by  # Agrupa los datos de una tabla en base a alguna de sus variables, de tal forma que las operaciones sobre los datos se lleven a cabo por grupos.
           # Podemos usar la función ungroup() si queremos deshacer una agrupación que hayamos realizado previamente.

# Ej.3 Averigua tu directorio de trabajo. 
#       Después crea una carpeta llamada B dentro de tu directorio. 
#       Situate en B (establecelo como directorio de trabajo)

  # 3.1) # Usamos la función getwd()

getwd() # Esta función "GET the Working Directory".
  
   #3.2) # Usamos la función dir.create("path/folder_name")

dir.create("C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/Día 2/B") 
        
  #3.3) Usamos la función setwd(path)

setwd("C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/Día 2/B")

  # Podemos cotejar que esta última operación se ha llevado a cabo con éxito mirando el cuadro de la consola, donde aparece el directorio de trabajo actual.

# Ej.4¿Cómo podemos acceder a los datos de la Comunidad Autonoma de Andalucia en la base 'agregados'?

  # Primero nos colocamos en el correspondiente directorio, cargamos el archivo y lo almacenamos en una variable:
  
  setwd("C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/Día 2")
  (tb = read.csv("agregados.csv", header = T)) # Ya que estamos le decimos que nos la muestre al guardarla. ¡Cuidado! Según la profesora no siempre lo 
                                               # importa como data.frame, mejor asegurar con as.data.frame.
  class(tb) # Obsérvese que el archivo ha sido guardado como un data.frame
  
  # Como Andalucía aparece como AN, para acceder a sus datos creamos una restricción CCAA == AN y, como es un data.frame, aprovechamos la función filter 
  # para aplicar la condición:
  
  (Andata <- filter(tb, CCAA == "AN"))
  
  # Belén lo hizo de la siguiente forma (con pipes):
  
  Andata2 <- tb %>% 
    filter(CCAA == "AN")

# Ej5. Importar la base de datos Gasto de turistas (Excel) manteniendo el nombre de las variables.
#       Accede a alguna de las variables presentes en esa base de datos y explora sus cualidades

  # Para esta tarea tenemos que usar la librería correspondiente, ya que ahora el documento es un Excel:
  
  library(readxl)
  
  # Ahora vamos a aplicar la función read_excel de esta librería para cargar el archivo pedido y almacenarlo en una variable:
  
  (tb2 <- read_excel('Gasto_de_los_turistas_segun_destino_principal.xlsx', col_names = T)) # Como vemos que el nombre de las variables se ha metido en las
                                                                                           # filas de datos, tenemos que usar un skip.
  
  (tb2 <- read_excel('Gasto_de_los_turistas_segun_destino_principal.xlsx', col_names = T, skip = 1)) # Ahora las variables están como encabezados (en su sitio).
  
  # Accedamos a algunas de las variables de la base de datos:
  
  class(tb2) # Como read_excel ha almacenado la tabla como data.frame, podemos usar select para ver las variables.
  
  select(tb2, CCAA, Gasto_2008) 
  
  # Para acceder a algunas de los descriptivos básicos de la base de datos podemos usar la función summary:
  
  summary(tb2)
  
  # Si queremos sus dimensiones podemos usar dim:
  
  dim(tb2)
  
  # Si queremos todas las variables junto a sus tipos, algunos datos y sus longitudes de columna; podemos usar str:
  
  str(tb2)
  
  # También podríamos acceder a características de una única variable en concreto, por ejemplo:
  
  str(tb2$Gasto_2005)
  summary(tb2$Gasto_2005)
  
# Ej6.Averigua las caracteristicas de la variable PCR del archivo agregados
  
  str(tb$PCR.)
  summary(tb$PCR.)
  class(tb$PCR.) # Si queremos solo la clase.

#Accede al elemento 2 de la variable

  tb$PCR.[2]
  
#Mostrar los elemento 3, 4 y 9

  tb$PCR.[c(3,4,9)]
  
#Mostrar todos los elementos menos el 5
  
  tb$PCR.[-5]

#Calcula la proporcion de datos que pertenecen a la Comunidad de Madrid

  cm_data_perc <- nrow(filter(tb, CCAA == "CM"))/nrow(tb) * 100; cm_data_perc
  (cm_data_perc2 <- sum(tb$CCAA == "CM")/length(tb$CCAA)*100) # Así lo hizo la profesora.
  
  #Lo he dado en % porque creo que es más visual para la mayoría que en tanto por 1.
  
# Ej.7 Observa el entorno de trabajo, 

#       - ¿En que directorio estan situados los objetos presentes?
# En ninguno; están en la memoria activa (en la RAM)
#       - borra alguno de los objetos presentes.
  
  rm(Andata)
