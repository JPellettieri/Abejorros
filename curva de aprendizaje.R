##Pregunta de investigacion:
#evaluar el efecto del tratamiento sobre el aprendizaje
#para cada # de tomas cuantos de los que tomaron extendieron la proboscide antes de  que les ofrezca la recompenza.

#VR: tasa de aprendizaje
#VE: Numero de exposicion al olor
    #Tratamiento (CAF,ARG,CAFyARG,SIN CNA, SIN OLOR)
#Distribucion: binomial


######### vamo a provar una recomendacion de nustro amigo GPT #########
# Paquetes necesarios
library(tidyverse)

# Manipulación para encontrar la primera ingesta de recompensa (T) y el primer aprendizaje (A) después de T
Datos <- Datos %>%
  rowwise() %>%
  mutate(
    Primera_T = which(c_across(starts_with("E")) == "T")[1],  # Primera ingesta de recompensa
    
    # Solo buscar 'A' si hubo una 'T'; si no, asignar NA
    Primera_A = if (!is.na(Primera_T)) {
      which(c_across(starts_with("E"))[Primera_T:length(c_across(starts_with("E")))] == "A")[1] + Primera_T - 1
    } else {
      NA
    }
  ) %>%
  ungroup()

# Verificación del nuevo dataframe
head(Datos)

# Resumen: Promedio de la primera exposición con aprendizaje por tratamiento
resumen_Aprendizaje <- Datos%>%
  group_by(Tratamiento) %>%
  summarise(
    Promedio_Primera_A = mean(Primera_A, na.rm = TRUE),
    SD = sd(Primera_A, na.rm = TRUE),
    N = n(),
    SE = SD / sqrt(N)
  )

# Ver el resumen
print(resumen_tratamiento)

# Visualización de las curvas de aprendizaje
curvas_aprendizaje <- Datos %>%
  group_by(Tratamiento, Primera_A) %>%
  summarise(N = n(), .groups = "drop") %>%
  group_by(Tratamiento) %>%
  mutate(Proporcion = cumsum(N) / sum(N))  # Proporción acumulada de aprendizaje

# Graficar las curvas de aprendizaje
curvas_aprendizaje %>%
  ggplot(aes(x = Primera_A, y = Proporcion, color = Tratamiento)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    x = "Exposición",
    y = "Proporción Acumulada de Aprendizaje",
    title = "Curvas de Aprendizaje por Tratamiento"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

