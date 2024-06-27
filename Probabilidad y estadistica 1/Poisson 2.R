#distribución de poisson
x=c(6) #número de éxitos
λ=4*3 #promedio por unidad de tiempo o región
P=sum(dpois(x,λ))
round(P,4)