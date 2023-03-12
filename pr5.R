numArtefactos = c (as.integer (17, 54, 10, 34, 90, 33, 49, 82, 12, 23, 56, 78, 44, 102, 10, 53, 4, 28, 37, 95))
numArtefactos = c(17, 54, 10, 34, 90, 33, 49, 82, 12, 23, 56, 78, 44, 102, 10, 53, 4, 28, 37, 95)
numArtefactos_int = as.integer (numArtefactos_int)
is.integer (numArtefactos_int)
is.vector(numArtefactos_int)

media1 = mean (numArtefactos_int)
media2 = mean (vector3)
mediana1 = median (numArtefactos_int)
mediana2 = median (vector3)

encontrar_moda = function (x) {
  
u = unique (x)
tab = tabulate (match(x, u))
u [tab == max (tab)]
}

moda1 = encontrar_moda (numArtefactos_int)

moda1

moda2 = encontrar_moda (vector3)

moda2


table (numArtefactos_int) 

quantnumArtefactos_int = quantile (numArtefactos_int)
quantnumArtefactos_int 

IQR (quantnumArtefactos_int) #da 40 El rango intercuartílico
#de estos valores es un rango en el que se corta el 25% a cada lado. Estadísticamente, el rango intercuartílico es la diferencia entre el cuartil superior y el cuartil inferior.
#40 es la diferencia entre 102 y 4



#calcular rango 

range (numArtefactos_int)
rango_artefactos = max (numArtefactos_int) - min (numArtefactos_int)

range (vector3)
rango_vector3 = max (vector3) - min (vector3)

#varianza

var1 = var (numArtefactos_int) #927.1026

var2 = var (vector3)




#otra forma de hacerlo (buscar y describirlas despues)

sd (numArtefactos_int) #desviación típica
sd (numArtefactos_int) ^2 #la varianza es tb el cuadrado de la desviacion tipica = 927.1026

#desviación estandar

sd (numArtefactos_int) #30.44836
sqrt (var (numArtefactos_int)) #30.44836

sd1 = sd(numArtefactos_int)
sd2 = sd(vector3)

#explicar la diferencia entre desviación estandar o típica y la varianza

#Visualiza gráficamente de manera horizontal la dispersión del objeto ‘numArtefactos_int’

plot(numArtefactos_int, pch = 19, col = "black")


vector3 = c (21, 45, 33, 98, 34, 90, 67, 87, 45, 11, 73, 38, 28, 15, 50, 57, 12, 87, 29, 1)

View (vector3)

vector3 = as.integer (vector3)

is.integer(vector3)
--


cv_numart <- sd(numArtefactos_int) / mean(numArtefactos_int) * 100
View (cv_numart)

cv_v3 <- sd(vector3) / mean(vector3) * 100
View (cv_v3)


#create data frame
coefvar <- data.frame (a =numArtefactos_int,
                     b = vector3)

#calcular CV para cada columna en el marco de datos
sapply (coefvar, function (x) sd (x) / mean (x) * 100 )


#a        b 
#66.84602 63.59067 



#Genera una tabla-resumen
#de los estadísticos descriptivos expuestos: media, mediana, desviación estándar etc. 


df1 = data.frame(numArtefactos_int, vector3)

df2 = data.frame(media1, media2, mediana1, mediana2, moda1, rango_artefactos, rango_vector3, var1, var2, sd1, sd2)

df1 = data.frame(estadisticos_numart = c(media1, mediana1, rango_artefactos, var1, sd1, cv_numart))

View (df1)

df2 = data.frame(estadisticos_v3 = c(media2, mediana2, rango_vector3, var2, sd2, cv_v3))

dataf = data.frame(df1, df2)

View (dataf)

table (dataf)

#. Calcula el coeficiente de asimetría del objeto ‘vector3’. Interpreta su resultado. 
#Exponga ejemplos de distribuciones de variables con asimetría positiva y negativa y simétricas. 
#Explique cada uno de estos escenarios.
  
# Asimetría = Simetría con respecto a su media
# Curtosis(Apuntamiento)= Mide cómo de achatada o apuntada es la curva
# y cómo se agrupan valores en torno a la media
  

install.packages("moments")
library(moments)

#Calcular el sesgo
skewness(vector3) #0.3389539
  
  
  
  
#Calcula la curtosis del objeto ‘vector3’. 
#¿Qué tipo de curtosis se encuentra asociada al anterior objeto? Justifica tu respuesta. 

kurtosis (vector3) #1.952376



--- #test jarque
  
jarque.test(vector3)
#Jarque-Bera Normality Test

#data:  vector3
#JB = 1.2976, p-value = 0.5227
#alternative hypothesis: greater

#El valor p de la prueba resulta ser 0.05756 . 
#Dado que este valor no es menor que α = .05, no rechazamos la hipótesis nula. 
#No tenemos evidencia suficiente para decir que este conjunto de datos tiene una asimetría y una curtosis diferente a la distribución normal

#Dado que la asimetría es negativa, esto indica que la distribución está sesgada a la izquierda.
#Esto confirma lo que vimos en el histograma.

#Dado que la curtosis es mayor que 3, esto indica que
#la distribución tiene más valores en las colas en comparación con una distribución normal.
