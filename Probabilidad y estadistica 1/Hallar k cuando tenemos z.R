# hallar k tal que P(x1<z<k)=p
p=0.3 #-->probabilidad de ser mayor que k
x1=29
m=30 #-->media
dx=2 #-->desviación de la variable x
p1=pnorm(x1, m,dx,lower.tail = T)
k=qnorm(p1+p,m,dx,lower.tail = T)
round(k,4)