rm(list = ls())
base = read.csv("heart_failure_clinical_records_dataset.csv",sep = ",",dec = ".", header =T)
tam=dim(base)
tam

n=200 #tamaño de muestra

set.seed(240823)

muestra=sample(1:299,n,replace=F) #seleccion de muestra
muestra

mu=base[muestra,]
# Hipotesis de una muestra 
#H0:mu=41
#Ha:mu<41
xbar = mean(mu$Fraccion.de.eyeccion)
xbar # media de la muestra 
mu0=41
o=sd(base$Fraccion.de.eyeccion)
o# es la desviacion estdar de la poblacion
n=200 # es el tamaño de la muestra 
alpha=0.05
zcal=(xbar-mu0)/(o/sqrt(n))
zcal
#Para el P-valor
p_value=pnorm(zcal,lower.tail = T)
p_value
if( p_value<alpha){
  " Los pacientes tienen la fracción de eyección menor a 41"
}else{
  "Los pacientes tienen la fracción de eyeccion igual a 41"
}

