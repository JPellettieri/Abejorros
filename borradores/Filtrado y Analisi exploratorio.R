#librerías
library(dplyr)
library(ggplot2)
library(readxl)

#Cargo datos
setwd("F:/Juli/Documents/Abejorros y Abejas/Abejorros")
Datos <- read_excel("Datos abejorros CNA-12-03-2025.xlsx",col_names = TRUE,  sheet = "dosis doble")
Datos$Tratamiento<- as.factor(Datos$Tratamiento)
summary (Datos$Tratamiento)

# Contar el número de veces que los abejorros tomaron la recompensa (T) en las exposiciones (E1 a E6) (o aprendieron "A")
#Datos <- Datos %>%
#  rowwise() %>%
#  mutate(Condicionamiento = sum(c_across(E1:E6) == "T" | c_across(E1:E6) == "A"))

# Filtrar para eliminar los registros con menos de 3 veces tomando la recompensa
Datos_filtrado <-Datos%>%
  filter(Ingestas >= 2)
summary (Datos_filtrado$Tratamiento)

# Calcular el índice de supervivencia según el tratamiento
Supervivencia <- Datos_filtrado %>%
  mutate(Grupo_Tratamiento = case_when(
    Tratamiento %in% c("CAF", "ARG", "CAF+ARG") ~ "CAF_ARG",  # Agrupar "CAF", "ARG", "CAF+ARG" juntos
    Tratamiento %in% c("SIN CNA", "SIN OLOR") ~ "SIN_CNA_OLOR",  # Agrupar "SIN CNA" y "SIN OLOR"
    TRUE ~ Tratamiento  # Mantener los otros tratamientos sin cambios si existieran
  )) %>%
  group_by(Grupo_Tratamiento) %>%
  summarise(Sobreviven = sum(Muere == 0), 
            Total = n(), 
            Supervivencia = Sobreviven / Total)

# Mostrar la tabla de supervivencia
print(Supervivencia)

# Graficar la supervivencia según el tratamiento
ggplot(Supervivencia, aes(x = Grupo_Tratamiento, y = Supervivencia)) +
  geom_bar(stat = "identity", fill ="orange") +
  labs(title = "Índice de Supervivencia según el Tratamiento",
       x = "Tratamiento",
       y = "Supervivencia (%)") +
  theme_minimal()

#-----------------------------------------------------------------------
# Filtrar para los que tomaron al menos 3 veces, sobrevivieron, y cumplieron con las condiciones de NONA
Filt_Cond_Sup_Esp <- Datos %>%
  filter(Condicionamiento >= 3, Muere == 0,`NONA 24 hs` == 0)

# Calcular el porcentaje de los que cumplen esas condiciones según el tratamiento
Respuesta <- Filt_Cond_Sup_Esp%>%
  group_by(Tratamiento) %>%
  summarise(Responden =  `LIO 24hs` == 1,
            Total = n_distinct(N),  # Reemplazar "Abejorro" por el nombre correcto de la columna
            Porcentaje = (Responden / Total) * 100)

# Mostrar la tabla de respuestas en porcentaje según el tratamiento
print(Respuesta)

# Graficar las respuestas en porcentaje según el tratamiento
library(ggplot2)
ggplot(Respuesta, aes(x = Tratamiento, y = Porcentaje)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Porcentaje de Abejorros que respondieron 1 en LIO 24hs y 0 en NONA 24hs",
       x = "Tratamiento",
       y = "Porcentaje (%)") +
  theme_minimal()


#-----------------------------
# Filtrar filas con NA en las variables relevantes
Filt_Cond_Sup_Esp_clean <- Filt_Cond_Sup_Esp %>%
  filter(!is.na(`LIO 24hs`), !is.na(Tratamiento), !is.na(Nido))


#------------------------------------------

# Asegurarte de que la columna LIO 24 hs sea numérica
Filt_Cond_Sup_Esp  <-Filt_Cond_Sup_Esp  %>%
  mutate(`LIO 24hs` = as.numeric(`LIO 24hs`))

## Modelo ##
#install.packages("lme4")
library(lme4)
# Ajustar el modelo mixto logístico
modelo_mixto <- glmer( Filt_Cond_Sup_Esp $`LIO 24hs` ~ Tratamiento + (1 | Nido), 
                      family = binomial, data = Filt_Cond_Sup_Esp )
 
