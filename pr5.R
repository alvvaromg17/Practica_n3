numArtefactos = c (as.integer (17, 54, 10, 34, 90, 33, 49, 82, 12, 23, 56, 78, 44, 102, 10, 53, 4, 28, 37, 95))
numArtefactos = c(17, 54, 10, 34, 90, 33, 49, 82, 12, 23, 56, 78, 44, 102, 10, 53, 4, 28, 37, 95)
numArtefactos_int = as.integer (numArtefactos_int)
is.integer (numArtefactos_int)
is.vector(numArtefactos_int)

mean (numArtefactos_int)
median (numArtefactos_int)

encontrar_moda = function (x) {
  
u = unique (x)
tab = tabulate (match(x, u))
u [tab == max (tab)]
}

encontrar_moda (numArtefactos_int)


table (numArtefactos_int) 

quantnumArtefactos_int = quantile (numArtefactos_int)
quantnumArtefactos_int 

IQR (quantnumArtefactos_int) #da 40 El rango intercuartílico
#de estos valores es un rango en el que se corta el 25% a cada lado. Estadísticamente, el rango intercuartílico es la diferencia entre el cuartil superior y el cuartil inferior.
#40 es la diferencia entre 102 y 4



#calcular rango 
max (numArtefactos_int, na.rm = TRUE ) - min (numArtefactos_int, na.rm = TRUE )
range (numArtefactos_int, na.rm = TRUE)
