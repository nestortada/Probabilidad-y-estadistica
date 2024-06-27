#P(x1 < X < x2)
x=c(29,30.551)
m=30 #-->media
dx=2 #-->desviación de la variable x
p2=pnorm(max(x),m,dx,lower.tail = T)
p1=pnorm(min(x),m,dx,lower.tail = T)
round(p2-p1,4)