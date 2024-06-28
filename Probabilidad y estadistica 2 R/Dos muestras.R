rm(list=ls())

datos= read.csv("heart_failure_clinical_records_dataset.csv",sep = ",",dec = ".", header =T)

#muestreo Estratificado
library(dplyr)
library(ggplot2)
install.packages("viridis")
library(viridis)

install.packages("hrbrthemes")
library(hrbrthemes)



#Creación de estratos
estr1 = filter(datos,(datos$sexo.mujer..0..hombre..1.)==0 & (datos$Anemia..0..no.si.1.)==0)
estr2 = filter(datos,(datos$sexo.mujer..0..hombre..1.)==0 & (datos$Anemia..0..no.si.1.)==1)
estr3 = filter(datos,(datos$sexo.mujer..0..hombre..1.)==1 & (datos$Anemia..0..no.si.1.)==0)
estr4 = filter(datos,(datos$sexo.mujer..0..hombre..1.)==1 & (datos$Anemia..0..no.si.1.)==1)
#Varianzas y desv
var1=var(estr1$Fraccion.de.eyeccion)
var2=var(estr2$Fraccion.de.eyeccion)
var3=var(estr3$Fraccion.de.eyeccion)
var4=var(estr4$Fraccion.de.eyeccion)

#Tamaño de estrato
N1= length(estr1$Fraccion.de.eyeccion)
N2=length(estr2$Fraccion.de.eyeccion)
N3=length(estr3$Fraccion.de.eyeccion)
N4=length(estr4$Fraccion.de.eyeccion)

#tamaño de cada muestra, 67%
n1=ceiling(N1*0.67)
n2=ceiling(N2*0.67)
n3=ceiling(N3*0.67)
n4=ceiling(N4*0.67)

set.seed(111)

m1=sample(1:N1,n1,replace=F)
mu1=estr1[m1,]
xbar1 = mean(mu1$Fraccion.de.eyeccion)

m2=sample(1:N2,n2,replace=F)
mu2=estr2[m2,]
xbar2 = mean(mu2$Fraccion.de.eyeccion)

m3=sample(1:N3,n3,replace=F)
mu3=estr3[m3,]
xbar3 = mean(mu3$Fraccion.de.eyeccion)

m4=sample(1:N4,n4,replace=F)
mu4=estr4[m4,]
xbar4 = mean(mu4$Fraccion.de.eyeccion)

#A dos muestras
#Anemia: mujer ,Anemia : 0 / Anemia: mujer , Anemia: 1

alfa = 0.05

z = qnorm(alfa/2,lower.tail = F)
Li1 = (xbar1-xbar2)-(z*(sqrt((var1/n1)+(var2/n2))))
Ls1 = (xbar1-xbar2)+(z*(sqrt((var1/n1)+(var2/n2))))
R1=c(Li1,Ls1)
R1

#Anemia: mujer, Anemia: 1 / Anemia: hombre , Anemia: 0
Li2 = (xbar2-xbar3)-(z*(sqrt((var2/n2)+(var3/n3))))
Ls2 = (xbar2-xbar3)+(z*(sqrt((var2/n2)+(var3/n3))))
R2=c(Li2,Ls2)
R2
#Anemia: Hombre Anemia: 0 / Anemia: Hombre Anemia: 1
Li3 = (xbar3-xbar4)-(z*(sqrt((var3/n3)+(var4/n4))))
Ls3 = (xbar3-xbar4)+(z*(sqrt((var3/n3)+(var4/n4))))
R3=c(Li3,Ls3)
R3
#Anemia: Mujer Anemia: 0 / Anemia: Hombre Anemia: 0
Li4 = (xbar2-xbar3)-(z*(sqrt((var2/n2)+(var3/n3))))
Ls4 = (xbar2-xbar3)+(z*(sqrt((var2/n2)+(var3/n3))))
R4=c(Li4,Ls4)
R4
#Anemia: MUjer Anemia: 0 / Anemia: Hombre Anemia: 1
Li5 = (xbar2-xbar4)-(z*(sqrt((var2/n2)+(var4/n4))))
Ls5 = (xbar2-xbar4)+(z*(sqrt((var2/n2)+(var4/n4))))
R5=c(Li5,Ls5)
R5
# Caja de Bigotes para Eyección de Fracción
B_Eyeccion = ggplot(mu4, aes(x = "Eyección de Fracción", y = Fraccion.de.eyeccion)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE) + 
  stat_summary(fun.y = mean, geom = "point", shape = 20, size = 4, color = "red", fill = "red") +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  ) +
  ggtitle("Caja de Bigotes de Eyección de Fracción") +
  xlab("")
B_Eyeccion

H_Eyeccion = ggplot(mu4, aes(x = Fraccion.de.eyeccion)) +
  geom_histogram(fill = "#69b3a2", color = "#e9ecef",binwidth = 8) + 
  ggtitle("Histograma de la Eyección de Fracción") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 15))
H_Eyeccion

