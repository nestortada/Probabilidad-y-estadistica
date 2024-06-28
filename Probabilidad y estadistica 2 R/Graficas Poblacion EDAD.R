rm(list=ls())
library(readxl)
library(ggplot2)
datos= read.csv("heart_failure_clinical_records_dataset.csv",sep = ",",dec = ".", header =T)


edadh <- ggplot(datos, aes(x = Edad)) +
  geom_histogram(color= "black", 
                 alpha=0.5,
                 fill="cyan",
                 binwidth = 4)+
  labs(x="Edad",
       y="Frecuencia")

edaddh=edadh+geom_vline(aes(xintercept=mean(Edad)),
                      color="red", linetype="dashed", size=1)
edaddh

H=ggplot()+
  geom_boxplot(data = datos, mapping = aes(x = Edad))+
  theme_classic()
H
