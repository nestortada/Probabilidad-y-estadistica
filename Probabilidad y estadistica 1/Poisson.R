#poisson exitos en intervalo de timepo y region , volumen : la esperanza de la distribucion 
#### landa
x=c(200) #número de éxitos
λ=25*7/1 #promedio por unidad de tiempo o región
P=sum(dpois(x,λ))
print(P)
