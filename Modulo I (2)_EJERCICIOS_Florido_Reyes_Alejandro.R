# Ej.1 Instalar el paquete ggplot2 con codigo
install.packages("ggplot2")

# Ej.2 Explorar el paquete dplyr con el siguiente codigo. �Que nos permite hacer cada una de sus funciones?
library("dplyr")
?select # Selecciona (y puede renombrar si se quiere) variables en un data.frame, tanto mediante �ndices como mediante funciones del tipo is.numeric.

?filter # Se usa para filtrar datos de un data.frame a trav�s de condiciones. Si la condici�n devuelve TRUE para un dato, dicho dato pasa el filtro.
        # Si se encuentra un NA en una de las filas que pasan el filtro, elimina dicha fila, as� que cuidado con eso, que no tiene en cuenta dichas filas y puede que 
        # el NA est� en una columna que, a priori, nos daba igual (es decir, estar�amos perdiendo informaci�n).

?arrange # Ordena las filas de un data.frame por sus valores en las columnas seleccionadas. A menos que se indiquen expl�citamente (o se a�ada 
         # .by_group = TRUE), este comando ignora las variables agrupadas (caso excepcional). Adem�s, las funciones de variables son evaluadas 
         # una vez por data.frame, no una vez por grupo.

?mutate  # Este comando a�ade nuevas variables y preserva las existentes, pero si dicha variable ya existe la sobreescribe, de modo que el comando sirve 
         # para a�adir nuevas variables a un conjunto y para modificar las ya existentes. Existe tambi�n otro comando (transmute) que crea nuevas variables
         # a costa de eliminar otras (dicho comando siempre elimina algo).

?summarise # Crea un nuevo data.frame con una o m�s filas para cada variable o combinaci�n de variables agrupadas. Si no hay variables agrupadas, el 
           # resultado ser� una �nica fila que resuma todas las observaciones sobre el data.frame. Por otro lado, habr� una columna para cada variable 
           # agrupada y cada valor estad�stico que hayamos espec�ficado en el argumento de la funci�n.

?group_by  # Agrupa los datos de una tabla en base a alguna de sus variables, de tal forma que las operaciones sobre los datos se lleven a cabo por grupos.
           # Podemos usar la funci�n ungroup() si queremos deshacer una agrupaci�n que hayamos realizado previamente.

# Ej.3 Averigua tu directorio de trabajo. 
#       Despu�s crea una carpeta llamada B dentro de tu directorio. 
#       Situate en B (establecelo como directorio de trabajo)

  # 3.1) # Usamos la funci�n getwd()

getwd() # Esta funci�n "GET the Working Directory".
  
   #3.2) # Usamos la funci�n dir.create("path/folder_name")

dir.create("C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/D�a 2/B") 
        
  #3.3) Usamos la funci�n setwd(path)

setwd("C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/D�a 2/B")

  # Podemos cotejar que esta �ltima operaci�n se ha llevado a cabo con �xito mirando el cuadro de la consola, donde aparece el directorio de trabajo actual.

# Ej.4�C�mo podemos acceder a los datos de la Comunidad Autonoma de Andalucia en la base 'agregados'?

  # Primero nos colocamos en el correspondiente directorio, cargamos el archivo y lo almacenamos en una variable:
  
  setwd("C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/D�a 2")
  (tb = read.csv("agregados.csv", header = T)) # Ya que estamos le decimos que nos la muestre al guardarla. �Cuidado! Seg�n la profesora no siempre lo 
                                               # importa como data.frame, mejor asegurar con as.data.frame.
  class(tb) # Obs�rvese que el archivo ha sido guardado como un data.frame
  
  # Como Andaluc�a aparece como AN, para acceder a sus datos creamos una restricci�n CCAA == AN y, como es un data.frame, aprovechamos la funci�n filter 
  # para aplicar la condici�n:
  
  (Andata <- filter(tb, CCAA == "AN"))
  
  # Bel�n lo hizo de la siguiente forma (con pipes):
  
  Andata2 <- tb %>% 
    filter(CCAA == "AN")

# Ej5. Importar la base de datos Gasto de turistas (Excel) manteniendo el nombre de las variables.
#       Accede a alguna de las variables presentes en esa base de datos y explora sus cualidades

  # Para esta tarea tenemos que usar la librer�a correspondiente, ya que ahora el documento es un Excel:
  
  library(readxl)
  
  # Ahora vamos a aplicar la funci�n read_excel de esta librer�a para cargar el archivo pedido y almacenarlo en una variable:
  
  (tb2 <- read_excel('Gasto_de_los_turistas_segun_destino_principal.xlsx', col_names = T)) # Como vemos que el nombre de las variables se ha metido en las
                                                                                           # filas de datos, tenemos que usar un skip.
  
  (tb2 <- read_excel('Gasto_de_los_turistas_segun_destino_principal.xlsx', col_names = T, skip = 1)) # Ahora las variables est�n como encabezados (en su sitio).
  
  # Accedamos a algunas de las variables de la base de datos:
  
  class(tb2) # Como read_excel ha almacenado la tabla como data.frame, podemos usar select para ver las variables.
  
  select(tb2, CCAA, Gasto_2008) 
  
  # Para acceder a algunas de los descriptivos b�sicos de la base de datos podemos usar la funci�n summary:
  
  summary(tb2)
  
  # Si queremos sus dimensiones podemos usar dim:
  
  dim(tb2)
  
  # Si queremos todas las variables junto a sus tipos, algunos datos y sus longitudes de columna; podemos usar str:
  
  str(tb2)
  
  # Tambi�n podr�amos acceder a caracter�sticas de una �nica variable en concreto, por ejemplo:
  
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
  (cm_data_perc2 <- sum(tb$CCAA == "CM")/length(tb$CCAA)*100) # As� lo hizo la profesora.
  
  #Lo he dado en % porque creo que es m�s visual para la mayor�a que en tanto por 1.
  
# Ej.7 Observa el entorno de trabajo, 

#       - �En que directorio estan situados los objetos presentes?
# En ninguno; est�n en la memoria activa (en la RAM)
#       - borra alguno de los objetos presentes.
  
  rm(Andata)
