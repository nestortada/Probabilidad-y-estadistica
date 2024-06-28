rm(list=ls())

datos= read.csv("heart_failure_clinical_records_dataset.csv",sep = ",",dec = ".", header =T)

#muestreo Estratificado
library(dplyr)
shapiro.test(datos$Fraccion.de.eyeccion) 
qqnorm(datos$Fraccion.de.eyeccion)
ks.test(datos$Fraccion.de.eyeccion)

#Creación de estratos
estr1 = filter(datos,(datos$sexo.mujer..0..hombre..1.)==0 & (datos$Anemia..0..no.si.1.)==0)
estr2 = filter(datos,(datos$sexo.mujer..0..hombre..1.)==0 & (datos$Anemia..0..no.si.1.)==1)
estr3 = filter(datos,(datos$sexo.mujer..0..hombre..1.)==1 & (datos$Anemia..0..no.si.1.)==0)
estr4 = filter(datos,(datos$sexo.mujer..0..hombre..1.)==1 & (datos$Anemia..0..no.si.1.)==1)
#Varianzas y desv
var1=var(estr1$Fraccion.de.eyeccion)
var2=var(estr2$Fraccion.de.eyeccion)
var3=var(estr3$Fraccion.de.eyeccion)
var4=var(estr4$Fraccion.de.eyeccion)

#Tamaño de estrato
N1= length(estr1$Fraccion.de.eyeccion)
N2=length(estr2$Fraccion.de.eyeccion)
N3=length(estr3$Fraccion.de.eyeccion)
N4=length(estr4$Fraccion.de.eyeccion)

#tamaño de cada muestra, 67%
n1=ceiling(N1*0.67)
n2=ceiling(N2*0.67)
n3=ceiling(N3*0.67)
n4=ceiling(N4*0.67)

set.seed(111)

m1=sample(1:N1,n1,replace=F)
mu1=estr1[m1,]
xbar1 = mean(mu1$Fraccion.de.eyeccion)

m2=sample(1:N2,n2,replace=F)
mu2=estr2[m2,]
xbar2 = mean(mu2$Fraccion.de.eyeccion)

m3=sample(1:N3,n3,replace=F)
mu3=estr3[m3,]
xbar3 = mean(mu3$Fraccion.de.eyeccion)

m4=sample(1:N4,n4,replace=F)
mu4=estr4[m4,]
xbar4 = mean(mu4$Fraccion.de.eyeccion)
alpha=0.05

#A dos muestras
#Anemia: mujer ,Anemia : 0 / Anemia: mujer , Anemia: 1
#Ho m1-m2=0
#Ha m1<m2
zcal=(xbar2-xbar4)/sqrt(((var2/n2)+(var4/n4)))
pvalue=pnorm(zcal,lower.tail = T)
if( pvalue<alpha){
  "Rechazo Ho"
}else{
  "No rechazo"
}
#Ho m1-m2=0
#Ha m1>m2
zcal=(xbar2-xbar4)/sqrt(((var2/n2)+(var4/n4)))
pvalue1=pnorm(zcal,lower.tail = F)
if( pvalue1<alpha){
  "Rechazo Ho"
}else{
  "No rechazo"
}







