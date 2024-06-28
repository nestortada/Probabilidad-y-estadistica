# 1 ejemplo 
n1=100
n2=120
p1=60/n1
p2=72/n2
q1=1-p1
q2=1-p2
a=0.05
vc=qnorm(a/2,lower.tail = F)
li=(p1-p2)-vc*sqrt((p1*q1/n1)+(p2*q2/n2))
ls=(p1-p2)+vc*sqrt((p1*q1/n1)+(p2*q2/n2))
R=c(li,ls)
R
# 2 ejemplo 
N1=900
N2=1000
P1=100/N1
P2=80/N2
Q1=1-P1
Q2=1-P2
A=0.05
VC= qnorm(A/2,lower.tail = F)
Li = (P1-P2)-VC*sqrt((P1*Q1/N1)+(P2*Q2/N2))
Ls= (P1-P2)+VC*sqrt((P1*Q1/N1)+(P2*Q2/N2))
r=c(Li,Ls)
r
#ejemplo 3
m1=100
m2=120
pro1=60/m1
pro2=72/m2
f1=1-pro1
f2=1-pro2
alpha=0.05
v_critico = qnorm(alpha/2,lower.tail = F)
LI=(m1-m2)-(v_critico*sqrt((pro1*f1/m1)+(pro2*f2/m2)))
LS=(m1-m2)+v_critico*sqrt((pro1*f1/m1)+(pro2*f2/m2))
respuesta = c(LI,LS)
respuesta

###tarea##

alp =0.05
pinturaA= c(3.5,2.7,3.9,4.2,3.6,2.7,3.3,5.2,4.2,2.9,4.4,5.2,4.0,4.1,3.4)
pinturaB= c(4.7,3.9,4.5,5.5,4.0,5.3,4.3,6.0,5.2,3.7,5.5,6.2,5.1,5.4,4.8)
t1=length(pinturaA)
t1
t2=length(pinturaB)
t2
s1=var(pinturaA)
s2=var(pinturaB)
F1=qf(alp/2,t1-1,t2-1,lower.tail = F)
F2=qf(1-(alp/2),t1-1,t2-1,lower.tail = F)
limi=(s1/s2)*(1/F1)
lims=(s1/s2)*(1/F2)
Rtarea = c(limi,lims)
Rtarea
# Son iguales las varianzas dado que el intervalo de confianza del 95% dentro de ese intervalo esta el 1