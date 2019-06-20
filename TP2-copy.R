X11()

library(ggplot2)


#Ej 1
momentosUniforme <- function(data){
  prom <- mean(data)
  return(2*prom)
}
EMVUniforme <- function(data){
  
  return(max(unlist(data)))
}
#Ej 2
bmed <- function(data){
  
  return(2*(median(data)))
}
#Ej 3

ejercicio3 <-function(){
  muestra <- runif(15,0,1)
  em <- momentosUniforme(1,muestra)
  print(paste0("Estimador de Momentos " , em))
  emv <- EMVUniforme(muestra)
  print(paste0("EMV = ",emv))
  bm <- bmed(muestra)
  print(paste0("BMed = ",bm))
  print(paste0('Error= ',em-emv ))
}

#Ej 4
ejercicio4 <- function(){
  Nrep <-1000
  bme <- c(1:Nrep)
  bmo <- c(1:Nrep)
  bmv <- c(1:Nrep)
  for (i in 1:Nrep) {
    muestra <- runif(15,0,1)
    bme[i] <- bmed(muestra)
    bmo[i] <- momentosUniforme(1,muestra)
    bmv[i] <- EMVUniforme(muestra)
  }
  #suma de muestreo
  for (i in 1:Nrep) {
    
    sumaBme = sumaBme + bme[i]
    sumaBmo = sumaBmo + bmo[i]
    sumaBmv = sumaBmv + bmv[i]
  }
  
  #sesgo
  sesgoBme = 1 - sumaBme / Nrep
  print(paste0("Sesgo Bme = ",sesgoBme))
  sesgoBmo = 1 - sumaBmo / Nrep
  print(paste0("Sesgo Bmo = ",sesgoBmo))
  sesgoBmv = 1 - sumaBmv / Nrep
  print(paste0("Sesgo Bmv = ",sesgoBmv))
  
  #varianza
  
  for (i in 1:Nrep) {
    for (j in i:Nrep){
      varBme = varBme + (bme[i] - bme[j])^2 
      varBmo = varBmo + (bmo[i] - bmo[j])^2
      varBmv = varBmv + (bmv[i] - bmv[j])^2
    }
  }
  
  varianzaBme = 1/(Nrep^2) * varBme
  print(paste0("varianza Bme = ",varianzaBme))
  varianzaBmo = 1/(Nrep^2) * varBmo
  print(paste0("varianza Bmo = ",varianzaBmo))
  varianzaBmv = 1/(Nrep^2) * varBmv
  print(paste0("varianza Bmv = ",varianzaBmv))
  
  #ecm
  ecmBme = varianzaBme + sesgoBme^2
  print(paste0("ECM Bme = ",ecmBme))
  ecmBmo = varianzaBmo + sesgoBmo^2
  print(paste0("ECM Bmo = ",ecmBmo))
  ecmBmv = varianzaBmv + sesgoBmv^2
  print(paste0("ECM Bmv = ",ecmBmv))
}

#ejercicio 5
#return(list(sesgo = sesgoBmv,varianza=varianzaBmv,ecm=ecmBmv))
simulacion_mv <- function(b,n,Nrep=1000){
  bmv <- c(1:Nrep)
  for (i in 1:Nrep) {
    muestra <- runif(n,0,b)
    bmv[i] <- EMVUniforme(muestra)
  }
  sumaBmv<-0
  #suma de muestreo
  for (i in 1:Nrep) {
    sumaBmv = sumaBmv + bmv[i]
  }
  
  #sesgo
  
  sesgoBmv = 1 - sumaBmv / Nrep
  #print(paste0("Sesgo Bmv = ",sesgoBmv))
  
  #varianza
  varBmv=0
  for (i in 1:Nrep) {
    for (j in i:Nrep){
      varBmv = varBmv + (bmv[i] - bmv[j])^2
    }
  }
  
  varianzaBmv = 1/(Nrep^2) * varBmv
  #print(paste0("varianza Bmv = ",varianzaBmv))
  
  
  ecmBmv = varianzaBmv+sesgoBmv^2
  return(list(sesgo = sesgoBmv,varianza=varianzaBmv,ecm=ecmBmv))
  
}
#return(list(sesgo = sesgoBmo,varianza=varianzaBmo,ecm=ecmBmo))
simulacion_mom <- function(b,n,Nrep=1000){
  bmo <- c(1:Nrep)
  for (i in 1:Nrep) {
    muestra <- runif(n,0,b)
    bmo[i] <- momentosUniforme(muestra)
  }
  #suma de muestreo
  sumaBmo<-0
  for (i in 1:Nrep) {
    sumaBmo = sumaBmo + bmo[i]
  }
  
  #sesgo
  
  sesgoBmo = 1 - sumaBmo / Nrep
  #print(paste0("Sesgo Bmo = ",sesgoBmo))
  
  #varianza
  varBmo=0
  for (i in 1:Nrep) {
    for (j in i:Nrep){
      varBmo = varBmo + (bmo[i] - bmo[j])^2
    }
  }
  
  varianzaBmo = 1/(Nrep^2) * varBmo
  #print(paste0("varianza Bmo = ",varianzaBmo))
  
  ecmBmo = varianzaBmo+sesgoBmo^2
  return(list(sesgo = sesgoBmo,varianza=varianzaBmo,ecm=ecmBmo))
}
#return(list(sesgo = sesgoBme,varianza=varianzaBme,ecm=ecmBme))
simulacion_med <- function(b,n,Nerp=1000){
  bme <- c(1:Nrep)
  for (i in 1:Nrep) {
    muestra <- runif(n,0,b)
    bme[i] <- bmed(muestra)
  }
  #suma de muestreo
  sumaBme<-0
  
  for (i in 1:Nrep) {
    sumaBme <- sumaBme + bme[i]
  }
  
  #sesgo
  sesgoBme = 1 - sumaBme / Nrep
  #print(paste0("Sesgo Bme = ",sesgoBme))
  
  #varianza
  varBme=0
  for (i in 1:Nrep) {
    for (j in i:Nrep){
      varBme = varBme + (bme[i] - bme[j])^2
    }
  }
  
  varianzaBme = 1/(Nrep^2) * varBme
  #print(paste0("varianza Bme = ",varianzaBme))
  
  ecmBme = varianzaBme+sesgoBme^2
  return(list(sesgo = sesgoBme,varianza=varianzaBme,ecm=ecmBme))
}

#ejercicio 6
#me guardo los valores de b en este arreglo
N_6 = 15
bes = c(0.1)
current = 0.2
while (current < 2){
  bes<- append(bes,current)
  current<-current+0.1
}
calcularEj6 <- function(n,bes){
  sesgosmv<-c()
  sesgosmo<-c()
  sesgosmed<-c()
  varianzasmv<-c()
  varianzasmo<-c()
  varianzasmed<-c()
  ecmsmv<-c()
  ecmsmo<-c()
  ecmsmed<-c()
  
  for (i in 1:length(bes)){
    ecmBmv<-simulacion_mv(bes[i],n)
    sesgosmv<-append(sesgosmv,ecmBmv$sesgo)
    varianzasmv<-append(varianzasmv,ecmBmv$varianza)
    ecmsmv<-append(ecmsmv,ecmBmv$ecm)
    
    ecmBmo<- simulacion_mom(bes[i],n)
    sesgosmo<-append(sesgosmo,ecmBmo$sesgo)
    varianzasmo<-append(varianzasmo,ecmBmo$varianza)
    ecmsmo<-append(ecmsmo,ecmBmo$ecm)
    
    ecmBmed<-simulacion_med(bes[i],n)
    sesgosmed<-append(sesgosmed,ecmBmed$sesgo)
    varianzasmed<-append(varianzasmed,ecmBmed$varianza)
    ecmsmed<-append(ecmsmed,ecmBmed$ecm)
  }
  resultsesgo <- data.frame(b=bes,sesgosmv,sesgosmo,sesgosmed)
  resultvarianza <- data.frame(b=bes,varianzasmv,varianzasmo,varianzasmed)
  resultecms <- data.frame(b=bes,ecmsmv,ecmsmo,ecmsmed)
  
  return(list(sesgo=resultsesgo,varianza=resultvarianza,ecm=resultecms))
}
data_ej6<-calcularEj6(15,bes)
ej6_plotsesgo <-function(data_ej6){
  sesgo <-melt(data_ej6$sesgo,id.vars='b')
  ggplot(sesgo, aes(b,value, col=variable)) + geom_boxplot()
}

ej6_plotvarianza<-function(data_ej6){
  varianza<-melt(data_ej6$varianza,id.vars='b')
  ggplot(varianza, aes(b,value, col=variable)) + geom_boxplot()
}

ej6_plotecm <-function(data_ej6){
  ecm<-melt(data_ej6$varianza,id.vars='b')
  ggplot(ecm, aes(b,value, col=variable)) + geom_boxplot()
}


#ejercicio 7
b_6 = 1
nes = c(15,30,60,120,240)

calcularEj7 <- function(b,nes){
  ecmsmv<-c()
  ecmsmo<-c()
  ecmsmed<-c()
  
  for (i in 1:length(nes)){
    Bmv<-simulacion_mv(b,nes[i])
    ecmsmv<-append(ecmsmv,Bmv$ecm)
    
    Bmo<- simulacion_mom(b,nes[i])
    ecmsmo<-append(ecmsmo,Bmo$ecm)
    
    Bmed<-simulacion_med(b,nes[i])
    ecmsmed<-append(ecmsmed,Bmed$ecm)
  }
  result <- data.frame(n=nes,ecmsmv,ecmsmo,ecmsmed)
  return(melt(result,id.vars='n'))
}
data_ej7 = calcularEj7(b_6,nes)
ggplot(data_ej7, aes(n,value, col=variable)) + geom_line()


#ejercicio 8
muestra8= c(0.917,0.247,0.384, 0.530,0.798,0.912,0.096,0.684,0.394,20.1,0.769,0.137,0.352 ,0.332,0.670)
