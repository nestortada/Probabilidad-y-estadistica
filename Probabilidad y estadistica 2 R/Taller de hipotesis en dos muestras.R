#Taller hipotesis a dos muestras 
  #Primer punto
    # Encntrar si las varianzas son iguales o diefrentes 
rm(list = ls())
s1=32
s2=29
n1=150
n2=200
x1=198
x2=206
alpha = 0.05
fcal=(s1^2)/(s2^2)
pvalue= 2*pf(fcal,n1-1,n2-1,lower.tail = F)
falpha1= qf(1-(alpha/2),n1-1,n2-1,lower.tail = F)
falpha2= qf((alpha)/2,n1-1,n2-1,lower.tail = F)
if( pvalue<alpha){
  "Rechazo Ho"
}else{
  "No rechazo"
}
if(fcal<=falpha1){
  "Rechazo Ho"
}else{
  "No rechazo Ho"
}
if(fcal>=falpha2){
  "Rechazo Ho"
}else{
  "No rechazo Ho"
}
# las varianzas son iguales 
#t-student
spa=((s1^2)*(n1-1))+((s2^2)*(n2-1))
spi=n1+n2-2
sp2=spa/spi
sp=sqrt(sp2)
tcal=((x1-x2)-0)/(sp*sqrt((1/n1)+(1/n2)))
pvalue = 1-pt(tcal,n1+n2-2,lower.tail = F)
talpha=qt(alpha,n1+n2-2,lower.tail = F)
if( pvalue<alpha){
  "Rechazo Ho"
}else{
  "No rechazo"
}
if(tcal<=-talpha){
  "Rechazo Ho"
}else{
  "No rechazo Ho"
}

    #punto 2 
rm(list = ls())
n1=90
x1=8.5
s1=1.8
n2=80
x2=7.9
s2=2.4
alpha = 0.05
fcal=(s1^2)/(s2^2)
pvalue= 2*pf(fcal,n1-1,n2-1,lower.tail = F)
falpha1= qf(1-(alpha/2),n1-1,n2-1,lower.tail = F)
falpha2= qf((alpha)/2,n1-1,n2-1,lower.tail = F)
if( pvalue<alpha){
  "Rechazo Ho"
}else{
  "No rechazo"
}
if(fcal<=falpha1){
  "Rechazo Ho"
}else{
  "No rechazo Ho"
}
if(fcal>=falpha2){
  "Rechazo Ho"
}else{
  "No rechazo Ho"
}
# las varianzas son diferentes
vs = (((s1^2)/n1)+((s2^2)/n2))^2
viis=((s1^2)/n1)^2
viii=n1-1
vii=viis/viii
vids=((s2^2)/n2)^2
vidi=n2-1
vid=vids/vidi
vi=vii+vid
v=vs/vi
tcal=((x1-x2))/(sqrt((((s1^2)/n1)+((s2^2)/n2))))
pvalue=pt(tcal,v,lower.tail = F)
talpha=qt(alpha,v,lower.tail = F)
if( pvalue<alpha){
  "Rechazo Ho"
}else{
  "No rechazo"
}
if(tcal>=talpha){
  "Rechazo Ho"
}else{
  "No rechazo Ho"
}
  #Punto 3 
rm(list = ls())
n1=400
p1=166/n1
q1=1-p1
n2=380
p2=205/n2
p=(166+205)/(n1+n2)
q=1-p
alpha = 0.01
zcal=(p1-p2)/sqrt((p*q*((1/n1)+(1/n2))))
pvalue=pnorm(zcal,lower.tail = F)
zalpha = qnorm(alpha,lower.tail = F)
if( pvalue<alpha){
  "Rechazo Ho"
}else{
  "No rechazo"
}
if(zcal>=zalpha){
  "Rechazo Ho"
}else{
  "No rechazo Ho"
}



