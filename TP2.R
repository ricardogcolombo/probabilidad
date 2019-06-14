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