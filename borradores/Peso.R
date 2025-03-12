library(dplyr)
library(ggplot2)
library(readxl)
library(lme4)
library(ggplot2)     # La vamos a utilizar para graficas
library(dplyr)       # La vamos a utilizar para armar tablas resumen
library(car)         # La vamos a utilizar para analisis de algunos supuestos 
library(glmmTMB)     # La vamos a utilizar para ajustar el modelo 
library(DHARMa)      # La vamos a utilizar para analizar supuestos
library(performance) # La vamos a utilizar para analizar supuestos y más ...
library(emmeans)     # La vamos a utilizar para realizar comparaciones 
library(ggeffects)   # La vamos a utilizar para obtener (y graficar) los valores predichos por el modelo
library(sjPlot)      # Alternativa para ggeffcets y grafico de efectos aleatorios

#### Cargo datos #### 
setwd("F:/Juli/Documents/Abejorros y Abejas/Abejorros")
Datos <- read_excel("Datos.xlsx",col_names = TRUE,  sheet = "Limpio")
Datos$Tratamiento<- as.factor(Datos$Tratamiento)
summary (Datos$Tratamiento) # para ver el N de cada tratamiento
Datos$Nido<- as.factor(Datos$Nido)

########### Filtrado de datos ###############
# Contar el número de veces que los abejorros tomaron la recompensa (T) en las exposiciones (E1 a E6) (o aprendieron "A")
Datos <- Datos %>%
  rowwise() %>%
  mutate(Condicionamiento = sum(c_across(E1:E6) == "T" | c_across(E1:E6) == "A"))

#Defino variable respuesta, Recuerda
Datos <- Datos %>%
  rowwise() %>%
  mutate(Recuerda = if_else(`LIO 24hs` == 1 & `NONA 24 hs` == 0, 1, 0))





Datos$Peso[is.na(Datos$Peso)] <- median(Datos$Peso, na.rm = TRUE)
# Crear una columna con peso cuali
Datos <- Datos %>%
  mutate(
    Peso_cuali= case_when(
      Peso <= 0.1643 ~ "Bajo",                      # Peso menor o igual al 1st Qu.
      Peso > 0.1643 & Peso <= 0.2284 ~ "Medio",    # Peso entre 1st Qu. y 3rd Qu.
      Peso > 0.2284 ~ "Alto"                       # Peso mayor al 3rd Qu.
    )
  )
Peso<-Datos$Peso
Datos$Peso_quintil <- ntile(Peso, 3)
print(Peso_quintil)

Datos$Peso_quintil<- as.factor(Datos$Peso_quintil)
valores_corte <- quantile(Peso, probs = seq(0, 1, 0.2), na.rm = TRUE)
print(valores_corte)
table(Datos$Peso_cuali)




Filtrado<- Datos %>%
  filter(Tratamiento!="SIN OLOR", Nido !="4", Condicionamiento>=3, Muere==0)


# Resumir por tratamiento y peso
Respuesta <- Filtrado%>%
  group_by(Peso_quintil, Tratamiento) %>%
  summarise(
    Responden = sum (`LIO 24hs` == 1 & `NONA 24 hs` == 0),  # Casos específicos
    Total = n_distinct(N),                                # Número de individuos únicos
    Porcentaje = (Responden / Total) * 100,               # Porcentaje de respuesta
    .groups = "drop")

# Mostrar los resultados
print(Respuesta)

# Gráfico: Porcentaje de respuesta por tratamiento y quintil de peso
#library(ggplot2)

ggplot(Respuesta, aes(x = Peso_quintil, y = Porcentaje)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Porcentaje de Respuesta por Peso y Tratamiento",
       x = "
       Peso",
       y = "Porcentaje de Respuesta (%)",
       fill = "Tratamiento") +
  theme_minimal()

MMixto<- glmer (Recuerda~Tratamiento+Peso_quintil+ (1|Nido), data=Filtrado, family=binomial)

summary(MMixto)
em_means <- emmeans(MMixto, ~ Tratamiento+Peso_quintil, type = "response") #### Hace las comparaciones para modelo mixto diferenciando por trat
confint(em_means)

contrasts <- contrast(em_means, method = "pairwise")
summary(contrasts)
#### gráfico de comp.
em_means_df <- as.data.frame(em_means) # dataset del emmeans anterior

ggplot(em_means_df, aes(x = Tratamiento, y = prob, ymin = asymp.LCL, ymax = asymp.UCL, color = Peso_quintil)) +
  geom_point(size = 3) +                       # Puntos para las medias marginales ajustadas
  geom_errorbar(width = 0.2) +                 # Barras de error para los intervalos de confianza
  labs(x = "Tratamiento", y = "Probabilidad de memoria") +
  theme_minimal()


######################################
#Modelos mixto para ver efecto del peso como cuanti
MMixto<- glmer (Recuerda~Peso + (1|Nido) + Tratamiento, data=Filtrado, family=binomial)


contrasts <- contrast(em_means, method = "pairwise")
summary(contrasts)
#### gráfico de comp.
em_means_df <- as.data.frame(em_means) # dataset del emmeans anterior

# Obtener predicciones marginales
predicciones <- ggeffect(MMixto, terms = "Peso")

# Visualizar las predicciones
ggplot(predicciones, aes(x = Peso, y = predicted, color=Tratamiento)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  labs(
    title = "Probabilidad esperada en función del Peso",
    x = "Peso del abejorro",
    y = "Probabilidad de recordar"
  ) +
  theme_minimal()
anova(MMixto)
#las diferencias no son sig pero hay una tendencia consistente en todos los tratamientos, la varianza amenta mucho a pasando el peso promedio.

