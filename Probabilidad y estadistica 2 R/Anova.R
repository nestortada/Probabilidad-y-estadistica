# Datos
pintura <- factor(rep(c("Pintura 1", "Pintura 2", "Pintura 3", "Pintura 4"), each = 5))
resistencia <- c(128, 137, 135, 124, 141, 144, 133, 142, 146, 130, 133, 143, 137, 136, 131, 150, 142, 135, 140, 153)

# Crear un dataframe
df <- data.frame(pintura, resistencia)

# Realizar el ANOVA
res.aov <- aov(resistencia ~ pintura, data = df)
summary(res.aov)

