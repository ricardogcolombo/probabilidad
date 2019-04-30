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
generar_sobre = function(tam_album, tam_sobre,repetidos=FALSE){
  sobre <- sample(tam_album,tam_sobre,replace=repetidos)
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
cuantas_figuritas = function(tam_album, tam_sobre,repetidos=FALSE){
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
tam_muestra <-1000
nRepRusia <- c(1:tam_muestra)
for (i in 1:tam_muestra) {
  nRepRusia[i] = cuantas_figuritas(tam_rusia,tam_sobreRusia)
}
#print(nRepRusia)

probSobres = function(nRepRusia,limite,tam_muestra){
  valorProb <- 0
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
    probmenos1 <-probSobres(nRepRusia,i-1,tam_muestra)
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
  esp <- esperanza(nRepRusia,tam_muestra,minimo,maximo)
  esperanza2 <-0
  for(i in minimo:maximo){
    probmenos1<-probSobres(nRepRusia,i-1,tam_muestra)
    prob <-probSobres(nRepRusia,i,tam_muestra)-probmenos1
    
    esperanza2 <- esperanza2 + i*i*prob
  }
  espAl2<-esp*esp
  varianza<- esperanza2 - espAl2
  print(paste(c('la varianza es ',varianza)))
  desvio<-sqrt(varianza)
  return(desvio)
}

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
  nRepRusiaSinRepetidos[i] = cuantas_figuritas(tam_rusia,tam_sobreRusia)
}


print(paste(c("probabilidad de completar el album con 800 sobres es Sin figuritas repetidas", probSobres(nRepRusiaSinRepetidos,800,tam_muestra)), collapse = " "))

sobresNec <-  sobres_necesarios(nRepRusiaSinRepetidos,tam_muestra,67)
print(paste(c("para que la probabilidad de completar el album sin figuritas repetidas sea mayor o igual a 0.9 se necesitan ",sobresNec), collapse = " "))
esp<-esperanza(nRepRusiaSinRepetidos ,tam_muestra,min(nRepRusiaSinRepetidos),max(nRepRusiaSinRepetidos))
print(paste(c('La esperanza sin figuritas repetidas es ',esp,collapse=' ')))
print(paste(c('El desvio estandar sin figuritas repetidas es ',desvio(nRepRusiaSinRepetidos,tam_muestra)),collapse=' '))
hist(nRepRusiaSinRepetidos)