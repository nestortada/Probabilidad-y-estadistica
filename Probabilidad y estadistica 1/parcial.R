library(agricolae)
library(modeest)
library(readxl)

df <- read_excel("Parcial 1 km_galon_G6.xlsx")

x = df$km
n = length(x)
n
datos = summary(x)
datos
# Abre un archivo PNG para guardar el gráfico
png(filename = "histograma.png")

# Crea el histograma
hist(x, main = "desempeño en terminos de kilometros recorridos por galon ", xlab = "Km",
     breaks = seq(24, 42, by=3), right = F,
     col="blue", xlim=c(24,41), ylim = c(0,10))

# Cierra el archivo PNG
dev.off()


tf = table.freq(histograma)
tf
media = sum((tf$Lower+1.5)*tf$Frequency)/n
media
q = quantile(x, type=6)
q
ric = q[4]-q[2]
ric
liminf = q[2]-1.5*ric
limsup = q[4]+1.5*ric
lim = c(liminf,limsup)
lim
bp = boxplot(x, horizontal = T)

bp
