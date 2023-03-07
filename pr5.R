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

