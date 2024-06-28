rm(list = ls())
n=c(7.07,7.00,7.10,6.97,7.00,7.03,7.01,7.01,6.98,7.08)
x=length(n)
media=mean(n)
des=sd(n)
M0=7.0
alpha=0.05
#H0:mu=70
#Ha:mu>70
tcal=(media-M0)/(des/sqrt(x))
tcal
#Para el P-valor
p_value=2*pt(abs(tcal),x-1,lower.tail = F)
p_value
#region de rechazo
talpha=qt(alpha/2,x-1,lower.tail = F)
talpha
if( p_value<alpha){
  "Rechazo Ho"
}else{
  "No rechazo"
}
if(abs(tcal)>=talpha){
  "Rechazo Ho"
}else{
  "No rechazo Ho"
}

