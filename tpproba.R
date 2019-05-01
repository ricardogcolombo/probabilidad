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
generar_sobre = function(tam_album, tam_sobre,repetidos){
  # el booleano en replace hace que me de repetidos o no dentro del sobre
  sobre <- sample(tam_album,tam_sobre,replace=repetidos)
  return(sobre)
}

#ejercicio 4
pegar_sobre = function(album,  sobre){
  #recorro el sobre y pongo true en la posicion de la figurita
  for (i in sobre) {
    album[i] = TRUE
  }
  return(album)
}

#ejercicio 5
cuantas_figuritas = function(tam_album, tam_sobre,repetidos){
  estaCompleto <- FALSE
  album1 <- rep(FALSE,tam_album)
  contAlbum <- 0
  #mientras no este completo
  while(!estaCompleto){
    #genero uso sobre
    sobre = generar_sobre(tam_album, tam_sobre,repetidos)
    #pego las figuritas
    album1 <- pegar_sobre(album1,sobre)
    #contabilizo un sobre mas
    contAlbum <-contAlbum +1
    #chequeo si no esta completo el album
    estaCompleto <- album_lleno(album1)
  }
  return(contAlbum)
}

#ej 6
#Simule 3 veces el llenado de un album con 6 figuritas y sobres de 1 figurita
print(cuantas_figuritas(6,1))
print(cuantas_figuritas(6,1))
print(cuantas_figuritas(6,1))

#ej 7
probSobres = function(nRepRusia,limite,tam_muestra){
  valorProb <- 0
  #sumo los valores que dan menor al limite para obtener la probabilidad de que con el limite o menos se cumpla
  for (i in nRepRusia) {
    if (i <= limite){
      valorProb = valorProb + 1
    }
  }
  x <-valorProb/tam_muestra
  return(x)
}

#- La cantidad de sobres que hacen falta comprar para completar el album con probabilidad mayor o igual a 0.9.
sobres_necesarios = function(nRepRusia,tam_muestra,cantSobresInicial){
  proba<-0
  cantSobres<-cantSobresInicial
  while(proba<0.9){
    proba <- probSobres(nRepRusia,cantSobres,tam_muestra)
    cantSobres<-cantSobres+10
  }
  return(cantSobres)
}

#- El valor esperado de la cantidad de sobres necesarios para completar el ´album del mundial de Rusia.
esperanza = function(nRepRusia,tam_muestra,min,max){
  esperanza <-0
  for(i in min:max){
    #calculo la probabilidad puntual
    probmenos1 <-probSobres(nRepRusia,i-1,tam_muestra)
    #el de probabilidad menos 1 se lo resto al actual para que me de la puntual y no la acumulada
    prob <-probSobres(nRepRusia,i,tam_muestra)-probmenos1
    
    esperanza <- esperanza + i*prob
  }
  return(esperanza)
}

#itero y obtengo la probabilidad que necesite 1 sobre por 1, mas la que necesite 2, etc.
#- El desvio estandar de la cantidad de sobres necesarios para completar el ´album.
desvio = function(nRepRusia,tam_muestra){
  minimo<-min(nRepRusia)
  maximo<-max(nRepRusia)
  #calculo esperanza
  esp <- esperanza(nRepRusia,tam_muestra,minimo,maximo)
  esperanza2 <-0
  #calculo esperanza x^2
  for(i in minimo:maximo){
    probmenos1<-probSobres(nRepRusia,i-1,tam_muestra)
    prob <-probSobres(nRepRusia,i,tam_muestra)-probmenos1
    
    esperanza2 <- esperanza2 + i*i*prob
  }
  #obtengo la esperanza al cuadrado
  espAl2<-esp*esp
  #calculo la varianza
  varianza<- esperanza2 - espAl2
  print(paste(c('la varianza es ',varianza)))
  
  #obtengo el desvio a partir de la varianza
  desvio<-sqrt(varianza)
  return(desvio)
}

#tam rusia 670
#genero la muestra
tam_rusia <- 670
tam_sobreRusia <- 5
tam_muestra <-1000
nRepRusia <- c(1:tam_muestra)
for (i in 1:tam_muestra) {
  nRepRusia[i] = cuantas_figuritas(tam_rusia,tam_sobreRusia,FALSE)
}
#print(nRepRusia)

#-OBJETIVO 1=- La probabilidad de completar el ´album con 800 sobres o menos.
print(paste(c("probabilidad de completar el album con 800 sobres es", probSobres(nRepRusia,800,tam_muestra)), collapse = " "))

#OBJETIVO 2= La cantidad de sobres que hacen falta comprar para completar el album con probabilidad mayor o igual a 0.9.
sobresNec <-  sobres_necesarios(nRepRusia,tam_muestra,67)
print(paste(c("para que la probabilidad de completar el albun sea mayor o igual a 0.9 se necesitan ",sobresNec), collapse = " "))

#OBJETIVO 3= El valor esperado de la cantidad de sobres necesarios para completar el ´album del mundial de Rusia.
esp<-esperanza(nRepRusia ,tam_muestra,min(nRepRusia),max(nRepRusia))
print(paste(c('La esperanza es ',esp,collapse=' ')))

#OBJETIVO 4= El desvio estandar de la cantidad de sobres necesarios para completar el ´album
print(paste(c('El desvio estandar es ',desvio(nRepRusia,tam_muestra)),collapse=' '))
#Dibujo la distribucion
hist(nRepRusia)

#ejercicio 8

nRepRusiaSinRepetidos <- c(1:tam_muestra)
for (i in 1:tam_muestra) {
  nRepRusiaSinRepetidos[i] = cuantas_figuritas(tam_rusia,tam_sobreRusia,TRUE)
}

#-OBJETIVO 1=- La probabilidad de completar el ´album con 800 sobres o menos.
print(paste(c("probabilidad de completar el album con 800 sobres es Sin figuritas repetidas", probSobres(nRepRusiaSinRepetidos,800,tam_muestra)), collapse = " "))

#OBJETIVO 2= La cantidad de sobres que hacen falta comprar para completar el album con probabilidad mayor o igual a 0.9.
sobresNec <-  sobres_necesarios(nRepRusiaSinRepetidos,tam_muestra,600)
print(paste(c("para que la probabilidad de completar el album sin figuritas repetidas sea mayor o igual a 0.9 se necesitan ",sobresNec), collapse = " "))

#OBJETIVO 3= El valor esperado de la cantidad de sobres necesarios para completar el ´album del mundial de Rusia.
esp<-esperanza(nRepRusiaSinRepetidos ,tam_muestra,min(nRepRusiaSinRepetidos),max(nRepRusiaSinRepetidos))
print(paste(c('La esperanza sin figuritas repetidas es ',esp,collapse=' ')))

#OBJETIVO 4= El desvio estandar de la cantidad de sobres necesarios para completar el ´album
print(paste(c('El desvio estandar sin figuritas repetidas es ',desvio(nRepRusiaSinRepetidos,tam_muestra)),collapse=' '))
hist(nRepRusiaSinRepetidos)