# distribución hipergeometrica
N=19 #Tamaño de la población
n= 10 #tamaño de la muestra
k=5 #número de éxitos
x=c(0,1) #número de k seleccionados
P=sum(dhyper(x,k,N-k,n))
round(P,4)
y=c(0.0108,0.1084,0.3251,0.3715,0.1625,0.0217)
ex= sum(x*y)
ex

