rm(list = ls())
library(ggplot2)
library(lmtest)
library(nortest)
library(GGally)

datos <- read.csv("heart_failure_clinical_records_dataset.csv")
datos2= datos[,c(-2,-4,-6,-10,-11,-12,-13)]

datos3= datos[, c(-1,-2,-3,-4,-6,-7,-8,-10,-11,-12,-13)]




# Análisis de regresión lineal múltiple
modelo <- lm( datos$Fraccion.de.eyeccion ~., data = datos2)

# Métodos secuenciales
modelo_backward <- step(modelo, direction = "backward")
### Siguiente modelo
modelo_forward <- step(modelo, direction = "forward")
#Siguiente modelo
modelo_stepwise <- step(modelo, direction = "both")

# Gráficos
p <- ggplot(datos3, aes_string(x= datos3$Nivel.de.sodio.en.sangre, y=datos3$Fraccion.de.eyeccion)) + geom_point() + geom_smooth(method=lm)
print(p)

#REgresion lineal multiple 
modelo2 <- lm(datos3$Fraccion.de.eyeccion ~., data = datos3)
summary(modelo2)

# Correlacion y determinacion 
core=cor(datos3$Nivel.de.sodio.en.sangre,datos3$Fraccion.de.eyeccion, method = "pearson")
print(core)
dete= core^2
print(dete)

# Validación de supuestos
#Normalidad
lillie.test(modelo2$residuals)
#HOMOCEDATICIDAD
bptest(modelo2)
#Independncia 
dwtest(modelo2)
#Graficas 
ggpairs(datos3,lower = list(continous="smooth"),
        diag = list(continous="barDiag"))
    
hist(modelo2$residuals,col="blue")
qqnorm(modelo2$residuals)
qqline(modelo2$residuals,col="red")

#Prueba de significancia 
confint(modelo2, level = 0.95)

# Pruebas de significancia
anova(modelo2)


# Análisis de varianza de un factor
datos$sexo.mujer..0..hombre..1.=as.factor(datos$sexo.mujer..0..hombre..1.)


modelo_anova <- aov(lm(datos$Fraccion.de.eyeccion ~ datos$sexo.mujer..0..hombre..1. ))
summary(modelo_anova)
## validación 

dwtest(modelo_anova)
#Prueba de normalidad
lillie.test(modelo_anova$residuals)
hist(modelo_anova$residuals)
qqnorm(modelo_anova$residuals)
qqline(modelo_anova$residuals,col="red")
#Prueba de Homocedasticidad
bptest(modelo_anova)
#Modelo Anova 

intervalos=TukeyHSD(modelo_anova)
intervalos
plot(intervalos)
