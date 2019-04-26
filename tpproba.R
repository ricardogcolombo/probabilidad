### Definir las siguientes funciones



album = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE) # es un ejemplo de un album de tam_album = 6 que tiene pegadas las figuritas 3 y 4
x1 <- sample(6,1)
album[x1] <- TRUE 
print(album)
print(x1)

## Esta completo album?
generar_sobre = function(tam_album, tam_sobre){
  sobre <- c(1:tam_sobre)
  for (l in 1:tam_sobre) {
    sobre[l] = sample(tam_album,1)
  }
  return(sobre)
}

pegar_sobre = function(album,  sobre){
  for (i in sobre) {
    album[i] = TRUE
  }
  return(album)
}

album_lleno = function(album){
  r <- TRUE
  for (i in 1:length(album)) {
    r = r && album[i]
  }
  return(r)
}

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

tam_rusia <- 670
tam_sobreRusia <- 5
nRepRusia <- c(1:1000)
for (i in 1000) {
  nRepRusia[i] = cuantas_figuritas(tam_rusia,tam_sobreRusia)
}
valorProb <- 0
for (i in nRepRusia) {
  if (i > 800){
    valorProb = valorProb + 1
  }
}
print(valorProb/1000)