#caracteristicas de lla normal en R

#para genelar numeros aleatorios con una distribución normal estandar 

random = rnorm(2004)
hist(random, col = "blue")
#calcular probabilidad
pnorm(random, lower.tail =T ) # T es igual a mano izquierda y F mano derecha 
#quantiles( valores criticos )
qnorm(random, lower.tail = )

#probabilidad en normal estandar 
j= pnorm(1,lower.tail = F)
qnorm(j, lower.tail = F)
j
### Ic para la media valor 

barx=2.6
sigma=0.3
n=36
vc=qnorm(0.025                                                                                                                                                                                                                                                                                                                                          , lower.tail=F)
vc
Li=barx-vc*sigma/sqrt(n)
Ls= barx+vc*sigma/sqrt(n)

IC=cbind(Li,Ls)
IC

e_e=vc*sigma/sqrt(n)
e_e

