#Taller de probabilidad 
# Primer puno
rm(list = ls())
  #H0:mu=2.5
  #Ha:mu=/2.5
zcal=(2.2-2.5)/(0.2/sqrt(42))
zcal
    #Para el P-valor
p_value=2*pnorm(abs(zcal),lower.tail = F)
p_value
    #region de rechazo
zalpha=qnorm(0.05,lower.tail = F)
zalpha
alpha=0.05
if( p_value<alpha){
  "Rechazo Ho"
}else{
  "No rechazo"
}
if(abs(zcal)>=zalpha/2){
  "Rechazo Ho"
}else{
  "No rechazo Ho"
}

  #Punto 2 
rm(list = ls())
    #H0:mu=44.95
    #Ha:mu<42.95

x=42.95
mu=44.95
o=5.75
n=25
zcal=(x-mu)/(o/sqrt(n))
zcal
      #Para el P-valor
p_value=1-pnorm(zcal,lower.tail = F)
p_value
#region de rechazo
alpha=0.02
zalpha=qnorm(alpha,lower.tail = F)
zalpha
if( p_value<alpha){
  "Rechazo Ho"
}else{
  "No rechazo"
}
if(zcal<=-zalpha){
  "Rechazo Ho"
}else{
  "No rechazo Ho"
}
  #Punto 3 
rm(list = ls())
    #H0:mu=28
    #Ha:mu>28

x=30.3
mu=28
s=16
n=64
tcal=(x-mu)/(s/sqrt(n))
tcal
#Para el P-valor
p_value=pt(tcal,n-1,lower.tail = F)
p_value
#region de rechazo
alpha=0.05
talpha=qt(alpha,n-1,lower.tail = F)
talpha
if( p_value<alpha){
  "Rechazo Ho"
}else{
  "No rechazo"
}
if(tcal>=talpha){
  "Rechazo Ho"
}else{
  "No rechazo Ho"
}
  #Punto 4 
  rm(list = ls())
    #H0:mu=6000/335
    #Ha:mu>6000/335
n=300
p0=250/n
p=0.8
q0=1-p0
zcal=(p-p0)/sqrt(p0*q0/n)
zcal
      #Para el P-valor
p_value= pnorm(abs(zcal),lower.tail = T)
p_value
#region de rechazo
alpha=0.04
zalpha=qnorm(alpha,lower.tail = T)
zalpha
if( p_value<alpha){
  "Rechazo Ho"
}else{
  "No rechazo"
}
if(abs(zcal)<=-zalpha){
  "Rechazo Ho"
}else{
  "No rechazo Ho"
}













