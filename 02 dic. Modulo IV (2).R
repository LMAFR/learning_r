###################MODULO IV cont...

#################************************Aclaracion Q-Q plot
#################*
#*El otro dia vimos que pnorm nos daba la probabilidad acumulada de un valor
#*Por ejemplo, la probabilidad de encontrar 0 o menos en la curva normal es 0.5
pnorm(0)
#*la funcion cuantil (quantile function) es la inversa: 
#*nos dira el valor que corresponde a una probabilidad dada
qnorm(0.5)
#es decir, el valor asociado al p-quantil nos indica que hay p probabilidad
#de obtener un valor menor o igual a el. La mediana,
#por tanto, es el 50% cuantil por definicion = nos indica que hay 50% de probabilidad
#de obtener un valor menor o igual 

#*(de hecho las tablas de probabilidad que se usaban antes para
#sacar conclusiones en los estudios, eran tablas de distribucion cuantil
#donde poder ver, dadas una serie de probabilidades, los valores que tendria
#que tener el estadistico calculado en el estudio (p.ej. t, F...)
#para ser considerado signifcativo) 

#*y es esta distribucion cuantil lo que representamos con los QQ plots
#*Ejemplo:
setwd("C:/Documentos y otros/EAE/Asignaturas y Master/Business Performance Analysis/Tras la reforma/Día 9")library(readxl)
gasto <- read_excel('Gasto_de_los_turistas_segun_destino_principal.xlsx',col_names = TRUE,skip=1)

gastoAN<- as.data.frame(t(gasto[1,4:17]))
qqnorm(gastoAN[,1], main="Gastos Andalucia");qqline(gastoAN[,1],distribution = qnorm)
#¿que estamos representando, por tanto, aqui?

#Dada cada probabilidad en la curva normal, representamos el valor asociado a esa probabilidad
#contra el valor encontrado con esa probabilidad
#por ejemplo, el valor 0 en la curva normal sabemos que tiene 0,5 de probabilidad acumulada
pnorm(0)
#ahora buscariamos el valor en nuestra muestra con probabilidad 0.5 y lo representariamos contra el 0
#¿en este caso cual sería?

#*La idea final es que, si mis datos siguen una distribucion normal,
#*tienen la misma probabilidad asociada que la distribucion teorica normal,
#*y por tanto la representacion seria una linea perfecta
#*
#################***********************FIN aclaracion Q-Q plot


#*Regresion lineal
#*
encuesta <- read.table("encuesta.dat", header=T, sep="\t",dec=',')
plot(encuesta$Edad,encuesta$TR_semaforo)
#al igual que con una sola v. cuantitativa, describimos:
#(1) forma: ¿pauta lineal?
#(2) centro: resumir la nube de puntos en una recta 
#(3) dispersión: concentración o alejamiento de los puntos a esa recta


lm(encuesta$TR_semaforo~encuesta$Edad) #* Intercept es la ordenada en el origen, el otro
                                       #* valor es la pendiente
#modelo lineal que estima tiempos de reaccion 
#en base a (~) la edad
#nos muestra que la recta que mejor
#representa nuestros datos es
### tiempo de reaccion= 0.2554 - 0.00009043 * edad
###¿que podemos interpretar?
#0.2554 es el tiempo de reaccion pronosticado para edad = 0
#por cada unidad que aumenta edad, la recta pronostica 
#un aumento de 0.00009043 en tiempo de reaccion
#¿y si B1 hubiera sido positivo? 

#la recta estaria pronosticando un aumento en el TR por cada aumento en la edad.

summary(lm(encuesta$TR_semaforo~encuesta$Edad))

#1)residuos: informacion sobre la parte de nuestra variable que 
#el modelo no es capaz de pronosticar (errores de prediccion)
#2)significacion de los coeficientes H0: B1 = 0
#nos fijamos solo en la segunda linea de la tabla (B1)
#B0 realmente actúa como término corrector para ajustar la métrica de la
#variable X a la de la variable Y, pero no aporta información sobre la relación entre X e Y
#3)grado de ajuste del modelo (multiple R squared)
#el % de varianza que el predictor explica de la VD
#(F en este caso testa la misma H0 que la t,
#sera informativo cuando haya mas de un predictor)

#* Nota: en una regresión múltiple Pr no sería igual que p, pero en una regresión única (como esta)
#* ambos son iguales.

# El adjusted R squared no nos interesa.
#¿conclusiones?
#no podemos decir que las variables esten significativamente relacionadas 
#de hecho, el coeficiente de determinacion nos dice que la edad solo explica 
#un 0.00002 de la varianza del tiempo de reaccion
plot(encuesta$TR_semaforo~encuesta$Edad)
abline(lm(encuesta$TR_semaforo~encuesta$Edad)) # Toma los valores de pte. y ordenada y dibuja la recta. 
# Vemos que, en efecto, es prácticamente horizontal (se ve horizontal, de hecho, aunque si
# hacemos zoom se ve algo más).

#**Pronosticos

