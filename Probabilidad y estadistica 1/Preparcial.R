library(agricolae)
x= c(61,95,133,69,137,118,138,60,165,42,120,121,158,41,58,133,111,249,60,86,134,159,64,60,69,114,48,47,104,164,85,77,104,61,156,65,150,148,61,100)
n=length(x)
n
datos=summary(x)
datos
#a) al indice de la calidad del aire, cuantitativa continua, y de razon#
png( filename = "histograma preparcial.png" )

histograma=hist(x,main = "Calidad del aire",xlab = "Pm2.5",
                breaks = seq(30, 250, by=20),right = F,
                col="blue",xlim=c(41,250),ylim = c(0,12))

dev.off()

tf=table.freq(histograma)
tf
media=sum((tf$Lower+10)*tf$Frequency)/n
media
mediana=90+(((n/2)-18)*20)/4
mediana
moda=50+(((11-4)*20)/((11-4)+(11-3)))
moda
#estan sesgados a la izquierda#
q=quantile(x, type=6)
q
ric=q[4]-q[2]
ric
liminf=q[2]-1.5*ric
limsup=q[4]+1.5*ric
lim=c(liminf,limsup)
lim
bp=boxplot(x, horizontal = T)
bp
vare=sd(x)
vare
