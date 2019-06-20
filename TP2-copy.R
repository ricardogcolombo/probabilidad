#Ej 1
momentosUniforme <- function(b,data){
  
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
em <- momentosUniforme(1,muestra)
print(paste0("Estimador de Momentos " , em))
emv <- EMVUniforme(muestra)
print(paste0("EMV = ",emv))
bm <- bmed(muestra)
print(paste0("BMed = ",bm))
print(paste0('Error= ',em-emv ))

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

simulacion_mv <- function(b,n){
  
  Nrep <-1000
  bmv <- c(1:Nrep)
  for (i in 1:Nrep) {
    muestra <- runif(15,0,1)
    bmv[i] <- EMVUniforme(muestra)
  }
  #suma de muestreo
  for (i in 1:Nrep) {
    
    sumaBmv = sumaBmv + bmv[i]
  }
  
  #sesgo

  sesgoBmv = 1 - sumaBmv / Nrep
  print(paste0("Sesgo Bmv = ",sesgoBmv))
  
  #varianza
  
  for (i in 1:Nrep) {
    for (j in i:Nrep){
      varBmv = varBmv + (bmv[i] - bmv[j])^2
    }
  }
  
  varianzaBmv = 1/(Nrep^2) * varBmv
  print(paste0("varianza Bmv = ",varianzaBmv))
  
  
  ecmBmv = varianzaBmv+sesgoBmv^2
  return(list(sesgo = sesgoBmv,varianza=varianzaBmv,ecm=ecmBmv))
  
}

simulacion_mom <- function(b,n){
  
  Nrep <-1000
  bmo <- c(1:Nrep)
  for (i in 1:Nrep) {
    muestra <- runif(15,0,1)
    bmo[i] <- EMVUniforme(muestra)
  }
  #suma de muestreo
  for (i in 1:Nrep) {
    
    sumaBmo = sumaBmo + bmo[i]
  }
  
  #sesgo
  
  sesgoBmo = 1 - sumaBmo / Nrep
  print(paste0("Sesgo Bmo = ",sesgoBmo))
  
  #varianza
  
  for (i in 1:Nrep) {
    for (j in i:Nrep){
      varBmo = varBmo + (bmo[i] - bmo[j])^2
    }
  }
  
  varianzaBmo = 1/(Nrep^2) * varBmo
  print(paste0("varianza Bmo = ",varianzaBmo))
  
  ecmBmo = varianzaBmo+sesgoBmo^2
  return(list(sesgo = sesgoBmo,varianza=varianzaBmo,ecm=ecmBmo))
  
}

simulacion_med <- function(b,n){
  
  Nrep <-1000
  bme <- c(1:Nrep)
  for (i in 1:Nrep) {
    muestra <- runif(15,0,1)
    bme[i] <- EMVUniforme(muestra)
  }
  #suma de muestreo
  for (i in 1:Nrep) {
    
    sumaBme = sumaBme + bme[i]
  }
  
  #sesgo
  
  sesgoBme = 1 - sumaBme / Nrep
  print(paste0("Sesgo Bme = ",sesgoBme))
  
  #varianza
  
  for (i in 1:Nrep) {
    for (j in i:Nrep){
      varBme = varBme + (bme[i] - bme[j])^2
    }
  }
  
  varianzaBme = 1/(Nrep^2) * varBme
  print(paste0("varianza Bme = ",varianzaBme))

  ecmBme = varianzaBme+sesgoBme^2
  return(list(sesgo = sesgoBme,varianza=varianzaBme,ecm=ecmBme))
}

