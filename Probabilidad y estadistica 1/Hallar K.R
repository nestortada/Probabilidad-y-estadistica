#P(X<K)=p o P(K<X)=p
p=0.15#-->probabilidad de ser mayor que o menor k
m=30 #-->media
dx=2 #-->desviación de la variable x
k=qnorm(p,m,dx,lower.tail = F) #lower.tail = T  cuando P(Z<k)
round(k,4)