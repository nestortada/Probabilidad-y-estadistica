rm(list = ls())
n=400
x=30
p=30/400
p
q=1-p
vc=qnorm(0.03/2,lower.tail = F)
li=p-vc*(sqrt((p*q)/n))
ls=p+vc*(sqrt((p*q)/n))
int=c(li,ls)
int
vc
n2=(((vc*vc)*p*q)/(0.02*0.02))
n2
