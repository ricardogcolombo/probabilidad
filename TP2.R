
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

muestra <- runif(15,0,1)
em <- momentosUniforme(muestra)
print(paste0("Estimador de Momentos " , em))
emv <- EMVUniforme(muestra)
print(paste0("EMV = ",emv))
bm <- bmed(muestra)
print(paste0("BMed = ",bm))
print(paste0('Error= ',em-emv ))

#Ej 4
ejercicio4 <- function(b,n,Nrep=1000){
  bme <- c(1:Nrep)
  bmo <- c(1:Nrep)
  bmv <- c(1:Nrep)
  for (i in 1:Nrep) {
    muestra <- runif(n,0,b)
    bme[i] <- bmed(muestra)
    bmo[i] <- momentosUniforme(muestra)
    bmv[i] <- EMVUniforme(muestra)
  }

  
  #sesgo
  sesgoBme = b - sum(bme) / Nrep
  print(paste0("Sesgo Bme = ",sesgoBme))
  sesgoBmo = b - sum(bmo) / Nrep
  print(paste0("Sesgo Bmo = ",sesgoBmo))
  sesgoBmv = b - sum(bmv) / Nrep
  print(paste0("Sesgo Bmv = ",sesgoBmv))
  
  #varianza
  varianzaBme =var(bme)
  print(paste0("varianza Bme = ",varianzaBme))
  varianzaBmo = var(bmo)
  print(paste0("varianza Bmo = ",varianzaBmo))
  varianzaBmv = var(bmv)
  print(paste0("varianza Bmv = ",varianzaBmv))
  
  #ecm
  ecmBme = varianzaBme + sesgoBme^2
  print(paste0("ECM Bme = ",ecmBme))
  ecmBmo = varianzaBmo + sesgoBmo^2
  print(paste0("ECM Bmo = ",ecmBmo))
  ecmBmv = varianzaBmv + sesgoBmv^2
  print(paste0("ECM Bmv = ",ecmBmv))
}
ejercicio4(1,15)
#ejercicio 5
#input: b limite uniforme, valor minimo es 0
#      n tamaño de la muestra,
#      Nrep repeticiones
#Output
# return list(sesgo = sesgoBmv,varianza=varianzaBmv,ecm=ecmBmv))
simulacion_mv <- function(b,n,Nrep=1000){
  bmv <- c(1:Nrep)
  for (i in 1:Nrep) {
    muestra <- runif(n,0,b)
    bmv[i] <- EMVUniforme(muestra)
  }
  
  #sesgo
  sesgoBmv = b - sum(bmv) / Nrep
  
  #print(paste0("Sesgo Bmv = ",sesgoBmv))
  
  #varianza
  varianzaBmv = var(bmv)
  #print(paste0("varianza Bmv = ",varianzaBmv))
  
  
  ecmBmv = varianzaBmv+sesgoBmv^2
  return(list(sesgo = sesgoBmv,varianza=varianzaBmv,ecm=ecmBmv))
  
}

#input: b limite uniforme, valor minimo es 0
#      n tamaño de la muestra,
#      Nrep repeticiones
#Output
#return(list(sesgo = sesgoBmo,varianza=varianzaBmo,ecm=ecmBmo))
simulacion_mom <- function(b,n,Nrep=1000){
  bmo <- c(1:Nrep)
  for (i in 1:Nrep) {
    muestra <- runif(n,0,b)
    bmo[i] <- momentosUniforme(muestra)
  }
  #sesgo
  sesgoBmo = b - sum(bmo) / Nrep
  #print(paste0("Sesgo Bmo = ",sesgoBmo))
  
  #varianza
  varianzaBmo = var(bmo)
  #print(paste0("varianza Bmo = ",varianzaBmo))
  
  ecmBmo = varianzaBmo+sesgoBmo^2
  return(list(sesgo = sesgoBmo,varianza=varianzaBmo,ecm=ecmBmo))
}
#input: b limite uniforme, valor minimo es 0
#      n tamaño de la muestra,
#      Nrep repeticiones
#Output
#return(list(sesgo = sesgoBme,varianza=varianzaBme,ecm=ecmBme))
simulacion_med <- function(b,n,Nerp=1000){
  bme <- c(1:Nrep)
  for (i in 1:Nrep) {
    muestra <- runif(n,0,b)
    bme[i] <- bmed(muestra)
  }

  #sesgo
  sesgoBme = b - sum(bme) / Nrep
  #print(paste0("Sesgo Bme = ",sesgoBme))
  
  #varianza
  varianzaBme = var(bme)
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
#input: 
#      n tamaño de la muestra,
#      bes array con valores para b
#Output
# return(list(sesgo=resultsesgo,varianza=resultvarianza,ecm=resultecms))
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
    #para cada b calculo los estmiadores, sesgos, varianzas y ecm
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
  #creo data frame para poder manejarlos mejor con ggplot
  resultsesgo <- data.frame(b=bes,sesgosmv,sesgosmo,sesgosmed)
  resultvarianza <- data.frame(b=bes,varianzasmv,varianzasmo,varianzasmed)
  resultecms <- data.frame(b=bes,ecmsmv,ecmsmo,ecmsmed)
  
  return(list(sesgo=resultsesgo,varianza=resultvarianza,ecm=resultecms))
}
data_ej6<-calcularEj6(15,bes)

ej6_plotsesgo <-function(data_ej6){
  sesgo <-melt(data_ej6$sesgo,id.vars='b')
  ggplot(sesgo, aes(b,value, col=variable)) + geom_line()+ggtitle("Sesgo de los estimadores")
}

ej6_plotvarianza<-function(data_ej6){
  varianza<-melt(data_ej6$varianza,id.vars='b')
  ggplot(varianza, aes(b,value, col=variable)) + geom_line()+ggtitle("Varianza de los estimadores")
}

ej6_plotecm <-function(data_ej6){
  ecm<-melt(data_ej6$ecm,id.vars='b')
  ggplot(ecm, aes(b,value, col=variable)) + geom_line()+ggtitle("ECM de los estimadores")
}
ej6_plotsesgo(data_ej6 )
ej6_plotvarianza(data_ej6 )
ej6_plotecm(data_ej6 )
data_ej6

#ejercicio 7
b_6 = 1
nes = c(15,30,60,120,240)

calcularEj7 <- function(b,nes){
  ecmsmv<-c()
  ecmsmo<-c()
  ecmsmed<-c()
  #para cada n calculo la simulacion y obtengo los estimadores
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
data_ej7

#ejercicio 8
muestra8= sort(c(0.917,0.247,0.384, 0.530,0.798,0.912,0.096,0.684,0.394,20.1,0.769,0.137,0.352 ,0.332,0.670))
ej8momentos<-momentosUniforme(muestra8)
ej8mv<- EMVUniforme(muestra8)
ej8med<-bmed(muestra8)
barplot(unlist(list(momentos=ej8momentos,maximavm=ej8mv,bmed=ej8med)),main='valor del estimador',horiz=TRUE,)
print(paste0("Estimador de Momentos Ej8 " , ej8momentos))
print(paste0("Estimador de maxima verosimilitud Ej8 " , ej8mv))
print(paste0("Estimador de bmed Ej8 " , ej8med))

#Ejercicio 9
muestra9 <- runif(15,0,1)

calcularEjercicio9 <-function(n=15,b=1,Nrep=1000,m,p=0.005){
  totalMal<-0
  bme <- c(1:Nrep)
  bmv <- c(1:Nrep)
  bmo <- c(1:Nrep)
  
  for(j in 1:Nrep){
    t<-rbinom(n,1,p)
    mAux<-m
    #contamino la muestra
    for(i in 1:n){
      if(t[i]==1){
        mAux[i]=mAux[i]*100
        totalMal=totalMal+1
      }
    }
    bme[j]<-bmed(mAux)
    bmv[j]<-EMVUniforme(mAux)
    bmo[j] <- momentosUniforme(mAux)
  }
  
  #Bmed
  sesgoBme = b - sum(bme) / Nrep
  varianzaBme = var(bme)
  ecmBme = varianzaBme+sesgoBme^2

  #Momentos
  sesgoBmo = b - sum(bmo) / Nrep
  varianzaBmo = var(bmo)
  ecmBmo = varianzaBmo+sesgoBmo^2
  
  #Maxima verosimilitud
  sesgoBmv = b - sum(bmv) / Nrep
  varianzaBmv = var(bmv)
  ecmBmv = varianzaBmv+sesgoBmv^2
  
  
  totalMal<-totalMal/1000
  print(totalMal)
  return(list(probabilidad=totalMal,sesgoBme=sesgoBme,varianzaBme=varianzaBme,ecmBme=ecmBme,sesgoBmo=sesgoBmo,varianzaBmo=varianzaBmo,ecmBmo=ecmBmo,sesgoBmv=sesgoBmv,varianzaBmv=varianzaBmv,ecmBmv=ecmBmv))
}
ej9<-calcularEjercicio9(m=muestra9,p=0.5)
#dentro de esta lista estan todos los valores(probabilidad, sesgos, varianzas y ecm para todos los estimadores)
ej9