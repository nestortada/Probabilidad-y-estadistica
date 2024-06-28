rm(list = ls())
datos=c(9.8,10.2,10.4,9.8,10.0,10.2,9.6)
x=mean(datos)
x
d_e=sd(datos)
d_e
vc=qt(0.005,7-1,lower.tail = F)
vc
Limite_inferior=x-vc*(d_e/sqrt(7))
Limite_superior=x+vc*(d_e/sqrt(7))
intervalo=c(Limite_inferior,Limite_superior)
intervalo
