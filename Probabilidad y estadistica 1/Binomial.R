#Distribución binomial
n=15 #número de ensayos
x=c(10:15) #valor(es) que toma la variable
p=0.78 #probabilidad de éxito
P=sum(dbinom(x,n,p))
round(P,4)

