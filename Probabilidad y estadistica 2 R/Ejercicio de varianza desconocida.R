rm(list = ls())
x=c(56,67,51,62,54,70,48,46,66,53,55,70,50,68,51,63,61,53,54,54,48,69,51,69,63,70,55,55,43,40,49,54,64,50,41,58,68,59,60,48,49,50,45,40,40,64,51,40)
media=mean(x)
d_e=sd(x)
vc=qt(0.005,length(x)-1,lower.tail = F)
Li= media-vc*(d_e/sqrt(length(x)))
Lf=media+vc*(d_e/sqrt(length(x)))
respuesta = c(Li,Lf)
respuesta  

