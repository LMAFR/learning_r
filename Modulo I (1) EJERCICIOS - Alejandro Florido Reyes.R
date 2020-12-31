# Ej01. Construir manualmente un vector, v, de longitud 10 con valores entre 0 y 10.

(v <- (0:9))
length(v)

#       - Mostrar el elemento 3

v[3]

#       - Mostrar los elemento 4 al 7

v[4:7]

#       - Mostrar los elemento 3, 4 y 9

v[c(3,4,9)]

#       - Mostrar todos los elementos menos el 5
v[c(1:4,6:10)]
  # Más fácil:
  v[-5]

# Ej02. Calcular la media del vector v

mean(v)

# Ej03. Calcular la varianza y dt del vector v

var(v)
sd(v)

# Ej04. Ejecuta estas lineas de codigo de matrices. Observa y explica que ocurre

ejemplo_matrix1 <- matrix(1:15, nrow = 3, ncol = 6) # Da warning porque hay más espacios que elementos a añadir en la matriz, por lo que se aplica una 
ejemplo_matrix1                                     # propiedad de R llamada concatenación que copia los primeros elementos añadidos hasta rellenar la 
                                                    # matriz.

(ejemplo_matrix2 <- matrix(1:15, nrow = 3, ncol = 5)) # Guarda los elementos de un vector que va del 1 al 15 de 1 en 1 (15 elementos) en tres filas y 5 
                                                      # columnas, empezando a rellenar la matriz por la columna 1, columna a columna.

ejemplo_matrix3 <- matrix(1:21, nrow = 6, ncol = 3) # Da warning porque hay más elementos a añadir que espacios en la matriz, por lo que se añaden solo 
ejemplo_matrix3                                     # aquellos que caben, siguiendo el orden del vector de datos (en este caso, del 1 al 21 de 1 en 1). 
                                                    

# Ej05. No se que hace la funcion 'split'. ¿Como lo averiguo?

# Escribo:

?split

# Ej06. Si quisieramos recoger los datos con las puntuaciones academicas de 4 alumnos en 4 tareas
#¿que objeto construiriais? Inténtalo con datos inventados

  # Modo fácil: creo una matriz (o, lo que equivalente, un array bidimensional) y le asigno nombres a las columnas y filas.
  # (comentario útil de David)Construiría una matriz ya que va a estar formada por un único tipo de dato:"numeric"

x <- c('Alejandro', 'Belen', 'Andrea', 'David')
y <- c('P1','P2','P3','P4')

(m1 <- matrix(c(0:10,0:4), nrow = 4, ncol = 4, dimnames = list(x,y)))

  # Modo cool: con un data frame.

(df6 <- data.frame(Alejandro = c(7:10), Andrea = c(7:10), Belen = c(7:10), David = c(7:10)))

View(df6) # Para visualizar el data frame en forma de tabla (más cool).

# Una buena idea: David usó sample() para generar las notas aleatoriamente.

# Ej07. Si quisieramos recoger las puntuaciones anteriores y las puntuaciones de 
#otros 4 alumnos de otra clase en las mismas tareas academicas
#¿que objeto construiriais?
#       - ¿Sabes seleccionar partes contiguas y discontiguas?
  
  # Tenemos tres dimensiones: clase a la que pertenecen los alumnos, nombre de alumno y número de tarea; por lo tanto, lo lógico sería usar un array de 3
  # dimensiones. Sin embargo, esto da problemas (véase el intento realizado al final del script) y por eso opté finalmente por una lista con dos matrices:


(m2 <- matrix(c(0:10,0:4), nrow = 4, ncol = 4, dimnames = list(z,y)))

clases = list(clase__1 = m1, clase__2 = m2); clases

clases$clase__2 # Alumnos y calificaciones de la clase 2. 

clases$clase__2[1,]     # Notas de Rebeca.
clases$clase__1[c(1,3),]     # Notas de Alejandro y Andrea.

clases$clase__1[2,3]    # Notas de Belen en la tarea 3.
clases$clase__1[c(2,4),c(2,3)] #Notas de Belen y David para las tareas 2 y 3.
  
  # Adicionalmente, puede encontrar intentos previos con data.frames y matrices al final de este script. 

# Ej08. Queremos almacenar las notas textuales de un curso (SS, AP, NOT, SOB, MH).
#       ¿Que objeto es el adecuado? Codifica 15 valores y ordenalos de menor a mayor.

notas <- c("SS", "AP","NOT", "SOB", "MH")
datos <-c(notas, notas, notas)
(f <-  factor(datos, levels = notas)) # Con esto, el orden queda definido. 
str(f) # Este comando nos muestra la estructura de f, de modo que podemos comprobar que, efectivamente, el orden se ha establecido como queremos.

sort(f) # Si, además, añadimos esta línea, el factor es reordenado atendiendo a la clasificación establecida en las líneas anteriores (antes solo 
        # habíamos establecido un orden a nivel interno, con sort() hacemos que el factor en sí se vea reordenado a nivel externo cuando lo llamamos).

  ## Ella usó la opción ordered = T para ordenarlos físicamente.

# Ej09. Tenemos los datos de 3 sujetos, que comprenden: nombre, apellidos,
#       edad, nota media en bachillerato y nivel de ingles (a1, a2, ..., c2).
#       ¿Que objeto es mas apropiado? Construyelo.

  # Yo utilizaría un data.frame, ya que queda más ordenado que una lista y podemos ir añadiendo los datos uno a uno conforme vayamos teniendo que aumentar 
  # el tamaño del data.frame, además, aunque tuviéramos muchos datos a introducir de una sola vez, a priori todos los campos van a ser completados en todos
  # los casos, así que no habrá problemas como que una columna sea independiente del resto y tenga un número de datos muy diferente a las demás, en cuyo caso
  # o escribimos manualmente "NA" para cada hueco o un data.frame podría dar comportamientos no deseados (error o copiar varias veces los datos que sí habíamos
  # introducido con el fin de completar la columna). En esos casos (véase el ejercicio 10), es mejor usar una lista.

a <- c("Alejandro", "Andrea", "Belen", "David")
b <- c("Florido Reyes", "Cifuentes Arroyo")
c <- c(24, 22)
d <- c(9.68, 8.89)
e <- c("C1", "B2")

(df <- data.frame(nombre = a, apellidos = b, edad = c, nota_media = d, nivel_de_ingles = e))


# Ej10. Queremos almacenar en un mismo objeto datos referentes a un colegio.
#       La informacion incluye el listado de profesores, de aulas, capacidades,
#       alumnos, nota media por asignatura y grupo, etc. ¿Objeto ideal?

  # Utilizaría una lista porque quiero añadir datos alfanuméricos, no solo numéricos y eso solo lo puedo hacer con listas o data.frames. 

  # Me decanto 
  # por una lista porque así no tengo que añadir manualmente 'NA', por ejemplo, por cada fila en la que no haya profesor (el nº total de profesores, por  
  # lo general, es menor que el nº total de alumnos), ya que un data.frame tiene un mecanismo que copia los nombres de las filas escritas previamente 
  # para completar el data.frame si el nº de datos que faltan por rellenar son múltiplo del número de filas completas o error si no lo son (a esto me 
  # refería en el comentario del ejercicio anterior).

  # Si no fuera por esto, utilizaría el data.frame, ya que se ve más ordenada la información, pero en este caso he priorizado poder llevar a cabo la tarea más
  # rápidamente (ya que se presupone una gran cantidad de datos y se necesitaría bastante tiempo para escribir el data.frame completo manualmente, al tener  
  # que añadir datos vacíos adicionales que la lista no requiere). 

  # Me gustaría añadir que creo que este problema surge cuando no hay una relación n:n*m (donde n y m son enteros) entre los 
  # datos de las diferentes columnas (el tipo de relación del ejercicio anterior, siendo en ese caso 1:1), por ejemplo, eso es lo que pasa en este ejercicio
  # entre alumnos y profesores: en un colegio no hay un profesor para cada alumno, sino un profesor para varios alumnos; de la misma forma ocurre entre aulas
  # y alumnos. 

# ---------------------------------------------------------------------------------------------------------------------------------------------

### Intento fallido del ejercicio 7 (no tuve en cuenta que había que señalar a qué clase pertenece cada grupo). Lo dejo aquí a modo de anécdota:

## Modo fácil

(m2 <- matrix(c(0:10,0:4), nrow = 4, ncol = 4, dimnames = list(z,y)))

(m3 <- rbind(m1,m2))

m3['Alejandro','P4']
m3['Belen',]
m3[,3]
m3[c(1,3),c(2,3)]

# -----------------------------------------------------------------------------------------

## Modo cool

df6$Rebeca <- c(6:9)
df6$Luis <- c(5,7,9,10)
df6$Carmen <- c(6:9)
df6$Pablo <- c(3:6)

df6
View(df6)

# Los controles para mostrar información específica son análogos a los usados para matrices (al menos a un nivel básico como el que estamos tratando ahora):

df6[1,3]
df6[1,]
df6[,1]
df6[c(1,3),c(2,3)]

# Aunque utilizar un data.frame puede quedar más bonito, con mis conocimientos actuales considero que las matrices son más fáciles de modificar y permiten
# crear una tabla con nombres de filas y columnas (en el data.frame he asumido que la numeración de las columnas corresponde al número de la tarea, no le 
# he asignado yo mismo un nombre a las filas)

# -----------------------------------------------------------------------------------------

## Con array

# Tenemos tres dimensiones: clase a la que pertenecen los alumnos, nombre de alumno y número de tarea; por lo tanto, usamos un array de 3 dimensiones.

z <- c('Rebeca', 'Luis', 'Carmen', 'Pablo')
w <- c(x,z)

ar <-array(data = c(0:10,0:10,0:9), dim = c(4,4,2), dimnames = list(x,y,c("Clase 1", "Clase 2"))); ar

# Para modificar los nombres en la segunda clase tenemos que dividir el array con una lista y entonces
# cambiar los nombres, de las filas de la clase 2, si no el programa no permite cambiar los nombres de esas filas en concreto.

ar_list = list(Clase_1 = ar[,,1], Clase_2 = ar[,,2])
rownames(ar_list[[2]]) <- z; ar_list

# Por lo tanto, usamos como base un array, pero el objeto final que creamos es una lista.

# Veamos cómo podemos acceder a las distintas partes de la lista que hemos creado:

ar_list[[2]] # Alumnos y calificaciones de la clase 2. 

# No los llamamos desde el array porque en el array los nombres de las filas no se han cambiado # de modo que nos serviría para llamar a la clase 1, pero no a la 2.

ar[1,,2]     # Notas de Rebeca.
ar[c(1,3),,1]     # Notas de Alejandro y Andrea.

ar[2,3,1]    # Notas de Belen en la tarea 3.
ar[c(2,4),c(2,3),1] #Notas de Belen y David para las tareas 2 y 3.

# El mayor inconveniente de hacerlo así es que no sé cómo llamar específicamente a los elementos, filas y columnas dentro  
# de la lista (me da errores si lo intento hacer de la forma habitual); de modo que tengo que para llamarlas uso el array 
# en lugar de la lista. Para llamar a filas o elementos sueltos no da problemas, pero si llamamos más de uno de ellos sí que 
# aparecen los nombres, lo que puede dar lugar a malentendidos con la clase 2, ya que en el array los nombres de esas filas 
# no los he podido modificar.
