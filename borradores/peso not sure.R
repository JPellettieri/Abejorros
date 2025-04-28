#Evaluar efecto peso

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

install.packages("lme4")

#### Cargo datos #### 
setwd("F:/Juli/Documents/Abejorros y Abejas/Abejorros")
Datos <- read_excel("Datos prueba.xlsx",col_names = TRUE,  sheet = "Limpio")
Datos$Tratamiento<- as.factor(Datos$Tratamiento)
summary (Datos$Tratamiento) # para ver el N de cada tratamiento
Datos$Nido<- as.factor(Datos$Nido)

########### Filtrado de datos ###############
# Contar el número de veces que los abejorros tomaron la recompensa (T) en las exposiciones (E1 a E6) (o aprendieron "A")
Datos <- Datos %>%
  rowwise() %>%
  mutate(Condicionamiento = sum(c_across(E1:E6) == "T" | c_across(E1:E6) == "A"))
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

# Convertir a factor para facilitar análisis y ordenación
Datos$Peso_cuali <- factor(Datos$Peso_cuali, levels = c("Bajo", "Medio", "Alto"))

# Verificar el resultado
table(Datos$Peso_cuali)




# Resumir por tratamiento y quintil de peso
Respuesta <- Datos%>%
  group_by(Peso_cuali, Tratamiento) %>%
  summarise(
    Responden = sum(`LIO 24hs` == 1 & `NONA 24 hs` == 0),  # Casos específicos
    Total = n_distinct(N),                                # Número de individuos únicos
    Porcentaje = (Responden / Total) * 100,               # Porcentaje de respuesta
    .groups = "drop"
  )

# Mostrar los resultados
print(Respuesta)

# Gráfico: Porcentaje de respuesta por tratamiento y quintil de peso
library(ggplot2)

ggplot(Respuesta, aes(x = Peso_cuali, y = Porcentaje)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Porcentaje de Respuesta por Peso y Tratamiento",
       x = "
       Peso",
       y = "Porcentaje de Respuesta (%)",
       fill = "Tratamiento") +
  theme_minimal()

