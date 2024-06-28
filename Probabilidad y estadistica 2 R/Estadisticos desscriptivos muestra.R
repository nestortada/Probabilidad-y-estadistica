rm(list=ls())
library(dplyr)
library(hrbrthemes)
library(viridis)

### Hsitogramas de distribucion utilizando ggplot de la mustra ##
mu= read.csv("heart_failure_clinical_records_dataset.csv",sep = ",",dec = ".", header =T)
# Gráfico de Densidad para Creatina Quinasa
Creatina_quinasa = ggplot(mu, aes(x = Creatina.Quinasa)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 0.5) +
  ggtitle("Distribución de los niveles de Creatina Quinasa") + theme_ipsum()
Creatina_quinasa

# Gráfico de Densidad para Eyección de Fracción
Eyeccion = ggplot(mu, aes(x = Fraccion.de.eyeccion)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 0.5) +
  ggtitle("Distribución de la Eyección de Fracción") + theme_ipsum()
Eyeccion

# Gráfico de Densidad para Plaquetas en Sangre
Plaquetas = ggplot(mu, aes(x = plaquetas.en.sangre)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 0.5) +
  ggtitle("Distribución de las Plaquetas en Sangre") + theme_ipsum()
Plaquetas

# Gráfico de Densidad para Nivel de Creatina en Sangre
Creatina_sangre = ggplot(mu, aes(x = Nivel.de.creatina.en.sangre)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 0.5) +
  ggtitle("Distribución del Nivel de Creatina en Sangre") + theme_ipsum()
Creatina_sangre

# Gráfico de Densidad para Nivel de Sodio en Sangre
Sodio_sangre= ggplot(mu, aes(x = Nivel.de.sodio.en.sangre)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 0.5) +
  ggtitle("Distribución del Nivel de Sodio en Sangre") + theme_ipsum()
Sodio_sangre

# Gráfico de Densidad para Tiempo de Análisis
Tiempo = ggplot(mu, aes(x =Timpo.de.analisis)) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 0.5) +
  ggtitle("Distribución del Tiempo de Análisis") + theme_ipsum()
Tiempo

### histogrmas para creatina de quinasa  ##
H_Creatina_quinasa = ggplot( mu, aes(x=Creatina.Quinasa)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.5) +
  ggtitle("Histogrma de los niveles de Creatina Quinasa ") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
H_Creatina_quinasa
# Histograma para Eyección de Fracción
H_Eyeccion = ggplot(mu, aes(x = Fraccion.de.eyeccion)) +
  geom_histogram(fill = "#69b3a2", color = "#e9ecef", alpha = 0.5) +
  ggtitle("Histograma de la Eyección de Fracción") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 15))
H_Eyeccion

# Histograma para Plaquetas en Sangre
H_Plaquetas = ggplot(mu, aes(x = plaquetas.en.sangre)) +
  geom_histogram(fill = "#69b3a2", color = "#e9ecef", alpha = 0.5) +
  ggtitle("Histograma de las Plaquetas en Sangre") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 15))
H_Plaquetas

# Histograma para Nivel de Creatina en Sangre
H_Creatina_sangre = ggplot(mu, aes(x = Nivel.de.creatina.en.sangre)) +
  geom_histogram(fill = "#69b3a2", color = "#e9ecef", alpha = 0.5) +
  ggtitle("Histograma del Nivel de Creatina en Sangre") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 15))
H_Creatina_sangre

# Histograma para Nivel de Sodio en Sangre
H_Sodio_sangre = ggplot(mu, aes(x = Nivel.de.sodio.en.sangre)) +
  geom_histogram(fill = "#69b3a2", color = "#e9ecef", alpha = 0.5) +
  ggtitle("Histograma del Nivel de Sodio en Sangre") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 15))
H_Sodio_sangre

# Histograma para Tiempo de Análisis
H_Tiempo = ggplot(mu, aes(x = Timpo.de.analisis)) +
  geom_histogram(fill = "#69b3a2", color = "#e9ecef", alpha = 0.5) +
  ggtitle("Histograma del Tiempo de Análisis") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 15))
H_Tiempo
# Histograma para edad
H_edad = ggplot(mu, aes(x = Edad)) +
  geom_histogram(fill = "#69b3a2", color = "#e9ecef", alpha = 0.5) +
  ggtitle("Histograma de edades") +
  theme_ipsum() +
  theme(plot.title = element_text(size = 15))
H_edad
# Histograma para Tiempo de Análisis
color=c("green","blue")
H_anemia=ggplot(mu, aes(x = Anemia..0..no.si.1.)) + geom_bar(fill=color) + labs(title = "Frecuanecia de anemia No(0) y Si(1)", y="frecuencia") + 
  theme_dark()
H_anemia
# Histograma de diabetes #
H_diabetes=ggplot(mu, aes(x =Diabetes)) + geom_bar(fill=color) + labs(title = "Frecuencia  de diabetes", y="frecuencia") + 
  theme_dark()
H_diabetes

# Histogrma para Presion alta 
H_presion=ggplot(mu, aes(x = Presion.alta.sangre)) + geom_bar(fill=color) + labs(title = "Frecuencia de pesion alta en sangre", y="frecuencia") + 
  theme_dark()
H_presion
# Histogrma para sexo 
col = c("pink","black")
H_sexo =ggplot(mu, aes(x =sexo.mujer..0..hombre..1.)) + geom_bar(fill=col) + labs(title = "Frecuencia de mujer o hombre", y="frecuencia") + 
  theme_dark()
H_sexo
# Histogrma para fumar
H_fumas=ggplot(mu, aes(x = Fumas)) + geom_bar(fill=color) + labs(title = "Frecuencia de fumar", y="frecuencia") + 
  theme_dark()
H_fumas
# Histogrma para Muerte 
H_muerte=ggplot(mu, aes(x = Muerte)) + geom_bar(fill=color) + labs(title = "Frecuencia de muertes", y="frecuencia") + 
  theme_dark()
H_muerte 

## cajas de bigotes ##
### Caja de bigotes para creatina de quinasa 
B_Crearina_quinasa= ggplot(mu, aes(x=Creatina.Quinasa)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Caja de Bigotes de creatina de quinasa") +
  xlab("")
B_Crearina_quinasa
# Caja de Bigotes para Eyección de Fracción
B_Eyeccion = ggplot(mu, aes(x = Fraccion.de.eyeccion)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  ) +
  ggtitle("Caja de Bigotes de Eyección de Fracción") +
  xlab("")
B_Eyeccion

# Caja de Bigotes para Plaquetas en Sangre
B_Plaquetas = ggplot(mu, aes(x = plaquetas.en.sangre)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  ) +
  ggtitle("Caja de Bigotes de Plaquetas en Sangre") +
  xlab("")
B_Plaquetas

# Caja de Bigotes para Nivel de Creatina en Sangre
B_Creatina_sangre = ggplot(mu, aes(x = Nivel.de.creatina.en.sangre)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  ) +
  ggtitle("Caja de Bigotes de Nivel de Creatina en Sangre") +
  xlab("")
B_Creatina_sangre

# Caja de Bigotes para Nivel de Sodio en Sangre
B_Sodio_sangre = ggplot(mu, aes(x = Nivel.de.sodio.en.sangre)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  ) +
  ggtitle("Caja de Bigotes de Nivel de Sodio en Sangre") +
  xlab("")
B_Sodio_sangre

# Caja de Bigotes para Tiempo de Análisis
B_Tiempo = ggplot(mu, aes(x =Timpo.de.analisis)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11)
  ) +
  ggtitle("Caja de Bigotes de Tiempo de Análisis") +
  xlab("")
B_Tiempo

