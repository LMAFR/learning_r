###################MODULO III (4)

#CUIDADO: antes de realizar cualquier analisis hay que comprobar los supuestos de dicho analisis

#************************************************************ANOVA de un factor
#*Vamos a ver si existen diferencias en el gasto de las comunidades de 
#*Andalucia, Baleares y Canarias desde el 2004 hasta el 2017
library(readxl)
gasto <- read_excel('Gasto_de_los_turistas_segun_destino_principal.xlsx',col_names = TRUE,skip=1)

variablegrupo<-rep(c("AN","IB","CA"),each=14) #*
gastoAN<- as.data.frame(t(gasto[1,4:17]))
gastoIB<- as.data.frame(t(gasto[2,4:17])) 
gastoCA<- as.data.frame(t(gasto[3,4:17])) 
gastos<- c(gastoAN$V1,gastoIB$V1,gastoCA$V1)
datos<- data.frame(variablegrupo,gastos); str(datos)
by(gastos,variablegrupo,mean)#miramos las medias que queremos comparar

anova(lm(gastos~variablegrupo)) #* La primera línea te devuelve lo que difiere el segundo término
                                #* de la fórmula para cada grupo y la segunda lo que difieren los
                                #* datos de cada muestra con respecto a la media para cada grupo.

#lm es el modelo que se usa para calcular el ajuste de los datos
#la primera linea nos habla de la variacion entre grupos
#la segunda nos habla de la variacion dentro de los grupos
#teniendo en cuenta ambas, nos da el estadistico F y su p-valor
#¿Conclusion? ¿entre que grupos?

#*Comparacion por pares:
#*
summary(lm(gastos~variablegrupo))
#nos da una tabla con t-test de cada grupo,
#cogiendo como referencia el primero (AN en este caso)
#PERO ESTO NO ES CORRECTO, así que vamos a hacer lo siguiente:

pairwise.t.test(gastos, variablegrupo, p.adj="bonferroni") 
#cuando hacemos multiples comparaciones, se aumenta la probabilidad
#de encontrar que una de ellas es significativa (los p valores se exageran)
#por lo que es necesario hacer la correccion de Bonferroni:
#basado en que la probabilidad de observar al menos un evento de entre n
#(en nuestro caso, de que una de las 3 comparaciones sea significativa)
#es menor de la suma de las probabilidades de cada evento
#por lo que multiplica cada p-valor por n
#Aunque existen otras correcciones, esta es la mas usada

#* De la corrección de Bonferroni obtenemos la información de que solo las medias de CA y AN
#* son significativamente diferentes y que es por eso en concreto por lo que rechazamos la 
#* hipótesis de que las tres medias sean iguales en el ANOVA (nos permite profundizar en los
#* resultados del ANOVA)

###################MODULO IV
#*Correlacion
#*diagrama de dispersión 
#como primera aproximación al estudio de la relación entre dos variables
encuesta <- read.table("encuesta.dat",header = T, dec = ",")
summary(encuesta)
plot(encuesta$FluidezVerbalEspanol,encuesta$FluidezVerbalIngles)

cor(encuesta$FluidezVerbalEspanol,encuesta$FluidezVerbalIngles)
#** ¡Cuidado! La función cor no funciona si hay NAs en la columna (a menos que usemos la opción)
#* use, como se hace ahora en un par de líneas de código.
#encuesta[1,6]<- NA
cor(encuesta$FluidezVerbalEspanol,encuesta$FluidezVerbalIngles)#requiere que no haya NAs
#o, en todo caso, hay que indicárselo:
cor(encuesta$FluidezVerbalEspanol,encuesta$FluidezVerbalIngles,use="complete.obs")
#**
#*
cor(encuesta[,6:9],use="complete.obs")#tambien se puede obtener una table
#¿que tipo de relaciones vemos? No hay una correlación muy alta entre la fluidez en español y en inglés.
#¿nos dicen algo de la poblacion?


cor.test(encuesta$FluidezVerbalEspanol,encuesta$FluidezVerbalIngles) #* Nos dice cómo de 
#* significativos son los valores de la correlación obtenida. Si p<0.05, se rechaza la hipótesis
#* de correlación = 0 (luego se entiende que hay cierta correlación).
#¿interpretacion? ¡Atención! La correlación NUNCA nos dice si hay una relación de causalidad entre
# dos variables. En este caso, podemos decir que hay una cierta correlación lineal (que no 
# causalidad) entre ambas variables.
#¿causalidad?

