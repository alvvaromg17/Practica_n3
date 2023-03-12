numArtefactos = c (as.integer (17, 54, 10, 34, 90, 33, 49, 82, 12, 23, 56, 78, 44, 102, 10, 53, 4, 28, 37, 95))
numArtefactos = c(17, 54, 10, 34, 90, 33, 49, 82, 12, 23, 56, 78, 44, 102, 10, 53, 4, 28, 37, 95)
numArtefactos_int = as.integer (numArtefactos)
is.integer (numArtefactos_int)
is.vector(numArtefactos_int)

#Media

media1 = mean (numArtefactos_int)
media2 = mean (vector3)

#Mediana 
mediana1 = median (numArtefactos_int)
mediana2 = median (vector3)

#Moda

encontrar_moda = function (x) {
  
u = unique (x)
tab = tabulate (match(x, u))
u [tab == max (tab)]
}

encontrar_moda

moda1 = encontrar_moda (numArtefactos_int)

moda1

---
table (numArtefactos_int) 
---
#Cuartiles
  
quantnumArtefactos_int = quantile (numArtefactos_int)
quantnumArtefactos_int 

# rango intercuartílico

IQR (quantnumArtefactos_int) #da 40 El rango intercuartílico
#de estos valores es un rango en el que se corta el 25% a cada lado. Estadísticamente, el rango intercuartílico es la diferencia entre el cuartil superior y el cuartil inferior.
#40 es la diferencia entre 102 y 4



#calcular rango 

range (numArtefactos_int)
rango_artefactos = max (numArtefactos_int) - min (numArtefactos_int)
rango_artefactos
range (vector3)
rango_vector3 = max (vector3) - min (vector3)

#varianza

var1 = var (numArtefactos_int) #927.1026

var2 = var (vector3)


#otra forma de hacerlo 

sd (numArtefactos_int) #desviación típica
sd (numArtefactos_int) ^2 #la varianza es tb el cuadrado de la desviacion tipica = 927.1026

#desviación estandar

sd (numArtefactos_int) #30.44836
sqrt (var (numArtefactos_int)) #30.44836

sd1 = sd(numArtefactos_int)
sd2 = sd(vector3)


#Visualiza gráficamente de manera horizontal la dispersión del objeto ‘numArtefactos_int’

plot1 = plot(numArtefactos_int, xlim =, xlab = "Index", ylab = "Vector numArtefactos_int", pch = 19, col = "black")

#Vector3

vector3 = c (21, 45, 33, 98, 34, 90, 67, 87, 45, 11, 73, 38, 28, 15, 50, 57, 12, 87, 29, 1)

View (vector3)

vector3 = as.integer (vector3)

is.integer(vector3)
--
#CV- forma 1

cv_numart <- sd(numArtefactos_int) / mean(numArtefactos_int) * 100
View (cv_numart)

cv_v3 <- sd(vector3) / mean(vector3) * 100
View (cv_v3)

#forma 2

#create data frame
coefvar <- data.frame (a =numArtefactos_int,
                     b = vector3)

#calcular CV para cada columna en el marco de datos
sapply (coefvar, function (x) sd (x) / mean (x) * 100 )


#a        b 
#66.84602 63.59067 


#Genera una tabla-resumen
#de los estadísticos descriptivos expuestos: media, mediana, desviación estándar etc.
df1 = data.frame(estadisticos_numart = c(media1, mediana1, rango_artefactos, var1, sd1, cv_numart))
df2 = data.frame(estadisticos_v3 = c(media2, mediana2, rango_vector3, var2, sd2, cv_v3))


dataf = data.frame(df1, df2)

row.names(dataf) = c ("Media", "Mediana", "Rango", "Varianza","Desviacion Estandar", "Coeficiente de Variacion")
colnames(dataf) = c("NumArtefactos", "Vector3")

View (dataf)

#Calcula el coeficiente de asimetría del objeto ‘vector3’.
  

install.packages("moments")
library(moments)

#Calcular el sesgo
skewness(vector3) #0.3389539

#Histograma
DV3 <- density(na.omit (vector3))
hist(vector3, prob = TRUE, main = "Histograma Vector3", col = "darkslateblue", ylab = "Densidad", xlab = "Valor", lty =1, lwd = 2)
lines (DV3, lwd = 3, col = "black")

#Calcula la curtosis del objeto ‘vector3’. 

kurtosis (vector3) #1.952376

--- #test jarque
  
jarque.test(vector3)
#Jarque-Bera Normality Test

#data:  vector3
#JB = 1.2976, p-value = 0.5227
#alternative hypothesis: greater
