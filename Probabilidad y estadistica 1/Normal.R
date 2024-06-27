#probabilidad de ser mayor o menor que
m=190 #media # si es muestral se divide en sqrt()
dx=20/sqrt(29) #desviación
p=pnorm(188.5,m,dx,lower.tail = T) # F mayor que - T menor que
round(p,4)