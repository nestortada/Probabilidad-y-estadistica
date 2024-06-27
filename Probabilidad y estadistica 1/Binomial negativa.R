#Binomial Negativa
x=c(778:834) #número de ensayos
k=1 #número de éxitos
p=4/20 #probabilidad de éxito
P=sum(dnbinom(x-k,k,p))
round(P,4)