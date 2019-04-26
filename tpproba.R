### Definir las siguientes funciones


#ejercicio 1
album = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE) # es un ejemplo de un album de tam_album = 6 que tiene pegadas las figuritas 3 y 4
x1 <- sample(6,1)
album[x1] <- TRUE 
print(album)
print(x1)

## Esta completo album?
#ejercicio2
album_lleno = function(album){
  r <- TRUE
  for (i in 1:length(album)) {
    r = r && album[i]
  }
  return(r)
}
#ejercicio 3
generar_sobre = function(tam_album, tam_sobre){
  sobre <- c(1:tam_sobre)
  for (l in 1:tam_sobre) {
    sobre[l] = sample(tam_album,1)
  }
  return(sobre)
}

#ejercicio 4
pegar_sobre = function(album,  sobre){
  for (i in sobre) {
    album[i] = TRUE
  }
  return(album)
}

#ejercicio 5
cuantas_figuritas = function(tam_album, tam_sobre){
  estaCompleto <- FALSE
  album1 <- rep(FALSE,tam_album)
  contAlbum <- 0
  
  while(!estaCompleto){
    sobre = generar_sobre(tam_album, tam_sobre)
    album1 <- pegar_sobre(album1,sobre)
    contAlbum <-contAlbum +1
    estaCompleto <- album_lleno(album1)
  }
  return(contAlbum)
}


#ej 6
print(cuantas_figuritas(6,1))
print(cuantas_figuritas(6,1))
print(cuantas_figuritas(6,1))

#ej 7
#tam rusia 670
#genero la muestra
tam_rusia <- 670
tam_sobreRusia <- 5
nRepRusia <- c(1:1000)
for (i in 1000) {
  nRepRusia[i] = cuantas_figuritas(tam_rusia,tam_sobreRusia)
}

#- La probabilidad de completar el ´album con 800 sobres o menos.
valorProb <- 0
for (i in nRepRusia) {
  if (i > 800){
    valorProb = valorProb + 1
  }
}
x <-valorProb/1000
print(paste(c("probabilidad de completar el album con 800 sobres es", x), collapse = " "))

#- La cantidad de sobres que hacen falta comprar para completar el album con probabilidad mayor o igual a 0.9.
valorProb <- 0
sobres <- 100
for (i in nRepRusia) {
  if (i > sobres){
    valorProb = valorProb + 1
  }
}
x<-valorProb/1000
print(paste(c("para que la probabilidad de completar el albun sea mayor o igual a 0.9 se necesitan ", sobres), collapse = " "))
print(paste(c("con 100 sobres la probabilidad es ", x), collapse = " "))

#- El valor esperado de la cantidad de sobres necesarios para completar el ´album del mundial de Rusia.
#- El desvio estandar de la cantidad de sobres necesarios para completar el ´album.

