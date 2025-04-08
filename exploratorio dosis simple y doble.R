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
library(sjPlot)   
# Analisis exploratorio integral
setwd("F:/Juli/Documents/Abejorros y Abejas/Abejorros")
Datos <- read_excel("Datos abejorros Integrados dosisi simple y doble 19_3_2025.xlsx",col_names = TRUE,  sheet = "Integración")
Datos$Tratamiento<- as.factor(Datos$Tratamiento)
summary (Datos$Tratamiento)

#Defino variable respuesta, Recuerda
Datos <- Datos %>%
  filter(Tratamiento != F)%>%
  rowwise() %>%
  mutate(Recuerda = if_else(`LIO 24hs` == 1 & `NONA 24 hs` == 0, 1, 0)) 
  

###Gráfico de % Resp vs canti de ingestas, discriminando por tratamiento
Vivos <- Datos %>%
  filter( Muere == 0)
Respuesta_general <-Vivos %>%
  group_by(Tratamiento, Ingestas) %>%
  summarise(
    Responden = sum(`LIO 24hs` == 1 & `NONA 24 hs` == 0),  	# Cuenta cuántos tienen LIO 24hs igual a 1
    Total = n_distinct(N),             	# Reemplaza "N" por el nombre correcto de la columna de ID única
    Porcentaje = (Responden / Total), # Calcula el porcentaje de respuesta
    .groups = "drop")
ggplot(Respuesta_general, aes(as.numeric(Ingestas), Porcentaje, colour = Tratamiento)) +
  geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) + 
  labs(title = "Porcentaje de respuesta a las 24hs en función\n de la ingesta discriminado por tratamiento",
       x = "Cantidad de ingestas",
       y = "Porcentaje de respuesta al PER 24hs") +
  scale_color_manual(values = c("SIN CNA" = "lightblue",
                                "CAF" = "#E69138",
                                "ARG" = "#8E7CC3",
                                "CAF+ARG" = "#C27BA0",
                                "2 CAF" = "orange",
                                "2 ARG" = "purple",
                                "2 CAF+ARG" = "#C27")) +
  theme(text = element_text(size = 14))

Ing <- Datos %>%
  filter(!is.na(Recuerda)) %>%
  group_by(Ingestas) %>%
  summarise(Probabilidad = mean(Recuerda)*100, n = n())
print (Ing)
ggplot(Ing, aes(x = as.factor(Ingestas), y = Probabilidad)) +
  geom_col(fill = "orange") +
  labs(title = "Probabilidad de recordar el olor a las 24hs en función\n de la cantidad de ingestas en el entrenamiento.",
       x = "Cantidad de ingestas",
       y = "Probabilidad de responder al PER (%)") +
  theme_minimal()



#Cambia el grafico si solo agarro el set de datos de los tratameintos de dosis simple o doble?
IngSimp <- Datos %>%
  filter(!is.na(Recuerda), Tratamiento %in% c("CAF", "ARG", "CAF+ARG")) %>%
  group_by(Tratamiento, Ingestas) %>%
  summarise(Probabilidad = mean(Recuerda) * 100, n = n(), .groups = "drop")

print(IngSimp)

ggplot(IngSimp, aes(x = as.factor(Ingestas), y = Probabilidad, fill = Tratamiento)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("CAF" = "#E69138", "ARG" = "#8E7CC6", "CAF+ARG" = "#C27BA0")) +
  labs(title = "Probabilidad de recordar el olor a las 24hs en función\n de la cantidad de ingestas en el entrenamiento.",
       x = "Cantidad de ingestas",
       y = "Probabilidad de responder al PER (%)",
       fill = "Tratamiento") +
  theme_minimal()



#Agrupo con dosis doble o dosis simple
IngestasAgrup <- Datos %>%
  filter(!is.na(Recuerda)) %>%
  mutate(Grupo_Tratamiento = case_when(
    Tratamiento %in% c("CAF", "ARG", "CAF+ARG") ~ "CAF_ARG",  # Agrupar "CAF", "ARG", "CAF+ARG" juntos
    Tratamiento %in% c("SIN CNA", "SIN OLOR") ~ "SIN_CNA_OLOR",  # Agrupar "SIN CNA" y "SIN OLOR"
    Tratamiento %in% c("2 CAF", "2 ARG", "2 CAF+ARG") ~ "2 CAF_ARG",
    TRUE ~ Tratamiento  # Mantener los otros tratamientos sin cambios si existieran
  )) %>%
  group_by(Grupo_Tratamiento) %>%
  summarise(Probabilidad = mean(Recuerda)*100, n = n())
print (IngestasAgrup)

ggplot(IngestasAgrup, aes(x = Grupo_Tratamiento, y = Probabilidad)) +
  geom_col(fill = "orange") +
  labs(title = "Probabilidad de recordar el olor a las 24hs en función\n dosis de enhancer.",
       x = "Grupo de Tratamiento",
       y = "Probabilidad de responder al PER (%)") +
  theme_minimal()
#--------------SUPERVIVENCIA--------------
#Indice de supervivencia segun tratamiento cuando hubo al menos 2 ingestas
Datos_filtrado <-Datos%>%
  filter(Ingestas >= 3)
summary (Datos_filtrado$Tratamiento)
Supervivencia <- Datos_filtrado %>%
  mutate(Grupo_Tratamiento = case_when(
    Tratamiento %in% c("CAF", "ARG", "CAF+ARG") ~ "CAF_ARG",  # Agrupar "CAF", "ARG", "CAF+ARG" juntos
    Tratamiento %in% c("SIN CNA", "SIN OLOR") ~ "SIN_CNA_OLOR",  # Agrupar "SIN CNA" y "SIN OLOR"
    Tratamiento %in% c("2 CAF", "2 ARG", "2 CAF+ARG") ~ "2 CAF_ARG",
    TRUE ~ Tratamiento  # Mantener los otros tratamientos sin cambios si existieran
  )) %>%
  group_by(Grupo_Tratamiento) %>%
  summarise(Sobreviven = sum(Muere == "0" ), 
            Total = n(), 
            Supervivencia = Sobreviven / Total)
print(Supervivencia)
# Graficar la supervivencia según el tratamiento
ggplot(Supervivencia, aes(x = Grupo_Tratamiento, y = Supervivencia)) +geom_bar(stat = "identity", fill ="orange") + labs(title = "Índice de Supervivencia según el Tratamiento",
       x = "Tratamiento",
       y = "Supervivencia (%)") + theme_minimal()


#SIN agrupar
SupervivenciaInd <- Datos_filtrado %>%
  group_by(Tratamiento) %>%
  summarise(Sobreviven = sum(Muere == "0" ), 
            Total = n(), 
            SupervivenciaInd = Sobreviven / Total)
print(SupervivenciaInd)
# Graficar la supervivencia según el tratamiento
ggplot(SupervivenciaInd,aes(x = Tratamiento, y = SupervivenciaInd, fill= Tratamiento)) +
  geom_bar(stat = "identity") +
  labs(title = "Índice de Supervivencia según el Tratamiento",
       x = "Tratamiento",
       y = "Supervivencia (%)") +
  theme_minimal()

#Agrupo segun compuesto
Datos_filtrado <-Datos%>%
  filter(Ingestas >= 3)
summary (Datos_filtrado$Tratamiento)
Supervivencia <- Datos_filtrado %>%
  mutate(Grupo_Comp = case_when(
    Tratamiento %in% c("CAF", "2 CAF") ~ "CAF",  # Agrupar "CAF", "ARG", "CAF+ARG" juntos
    Tratamiento %in% c("SIN CNA", "SIN OLOR") ~ "SIN_CNA_OLOR",  # Agrupar "SIN CNA" y "SIN OLOR"
    Tratamiento %in% c( "ARG", "2 ARG") ~ "ARG",
    Tratamiento %in% c( "CAF+ARG", "2 CAF+ARG") ~ "CAF+ARG",
    TRUE ~ Tratamiento  # Mantener los otros tratamientos sin cambios si existieran
  )) %>%
  group_by(Grupo_Comp) %>%
  summarise(Sobreviven = sum(Muere == "0" ), 
            Total = n(), 
            Supervivencia = Sobreviven / Total)
print(Supervivencia)
# Graficar la supervivencia según el tratamiento
ggplot(Supervivencia, aes(x = Grupo_Comp, y = Supervivencia)) +geom_bar(stat = "identity", fill ="orange") + labs(title = "Índice de Supervivencia según el Tratamiento",
                                                                                                                         x = "Tratamiento",
                                                                                                                  y = "Supervivencia (%)") + theme_minimal()

## Supervivencia en funcion de cantidad de ingestas                                                                                                                                                                                                                                           

SuperIng <- Datos %>%
  filter(!is.na(Muere))  %>%
  group_by(Ingestas) %>%
  summarise(Sobreviven = sum(Muere == "0" ), 
            Total = n(), 
            Supervivencia = Sobreviven / Total) %>%
  arrange(Ingestas)
  
print(SuperIng)

ggplot(SuperIng, aes(x = factor(Ingestas), y = Supervivencia)) +
  geom_col(fill = "#76A5AF") +
  labs(
    x = "Cantidad de ingestas",
    y = "Supervivencia (%)",
    title = "Supervivencia en función de la cantidad de ingestas"
  ) +
  theme_minimal()

#Supervivencia segun quintil peso 
Datos$Peso <- as.numeric((Datos$Peso))

Datos <- Datos %>%
  filter(!is.na(Muere), !is.na(Peso)) %>%
  mutate(
    QuintilPeso = cut_number(Peso, 5, labels = FALSE)  # esto crea los quintiles
  )

SuperPeso <- Datos %>%
  group_by(QuintilPeso) %>%
  summarise(
    Sobreviven = sum(Muere == "0"),
    Total = n(),
    Supervivencia = Sobreviven / Total
  ) %>%
  mutate(
    Supervivencia = round(Supervivencia * 100, 1),
    QuintilPeso = as.factor(QuintilPeso)
  )

ggplot(SuperPeso, aes(x = QuintilPeso, y = Supervivencia)) +
  geom_col(fill = "#FFD966") +
  geom_text(aes(label = Supervivencia), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 105)) +
  labs(
    x = "Quintil de peso",
    y = "Supervivencia (%)",
    title = "Supervivencia según quintiles de peso"
  ) +
  theme_minimal()





#___________MEMORIA 24hs______________#####
# Filtrar para los que tomaron al menos 3 veces, sobrevivieron, y cumplieron con las condiciones de NONA
Filt_Cond_Sup_Esp <- Datos %>%
  filter(Ingestas >= 3, Muere == 0,`NONA 24 hs` == 0)

# Calcular el porcentaje de los que cumplen esas condiciones según el tratamiento
Respuesta <- Filt_Cond_Sup_Esp %>%
  group_by(Tratamiento) %>%
  summarise(Responden = sum(`LIO 24hs` == 1),  # Contar cuántos tienen LIO 24hs == 1
            Total = n_distinct(N),  
            Porcentaje = (Responden / Total) * 100)

Respuesta <- Respuesta %>%
  filter(Tratamiento != "SIN OLOR")

# Graficar las respuestas en porcentaje según el tratamiento
library(ggplot2)
ggplot(Respuesta, aes(x = Tratamiento, y = Porcentaje, fill=Tratamiento)) +
  geom_bar(stat = "identity") +
  labs(title = "Porcentaje de Abejorros que respondieron 1 en LIO 24hs y 0 en NONA 24hs",
       x = "Tratamiento",
       y = "Porcentaje (%)") +
  scale_fill_manual(values = c("SIN CNA" = "lightblue",
                                "CAF" = "#E69138",
                                "ARG" = "#8E7CC3",
                                "CAF+ARG" = "#C27BA0",
                                "2 CAF" = "orange",
                                "2 ARG" = "purple",
                                "2 CAF+ARG" = "#C27"))+
  theme_minimal()
print(Respuesta)

#Modelo
DatosMod <- Datos %>%
  filter(Tratamiento != "SIN OLOR")
MMixtoNido<- glmer(Recuerda~Tratamiento+(1|Nido), data=DatosMod, family=binomial)
summary(MMixtoNido)
anova(MMixtoNido)
em_means <- emmeans(MMixtoNido, ~ Tratamiento, type = "response") #### Hace las comparaciones para modelo mixto diferenciando por trat
confint(em_means)


contrasts <- contrast(em_means, method = "pairwise")
summary(contrasts)

#####HAgo spagetti plot para evaluar los distintos nidos #########
# Calcular la tasa de aprendizaje promedio por tratamiento
TasaAprendizaje <- DatosMod %>%
  group_by(Tratamiento, Nido) %>%
  summarise(TasaPromedio = mean(Recuerda, na.rm = TRUE), .groups = "drop")
print(TasaAprendizaje)
# Spaghetti-plot de las tasas de aprendizaje por Nido
ggplot(TasaAprendizaje, aes(x = Tratamiento, y = TasaPromedio, colour = factor(Nido), group = Nido)) +
  labs(title = "Tasa Promedio de Aprendizaje por Tratamiento",
       x = "Tratamiento",
       y = "Tasa de Aprendizaje Promedio") +
  geom_point() +
  geom_line() +
  theme_bw()

### Spagetti plot por dia ###
TasaAprendizaje <- DatosMod %>%
  group_by(Tratamiento,Dia) %>%
  summarise(TasaPromedio = mean(Recuerda, na.rm = TRUE), .groups = "drop")
print(TasaAprendizaje)
#Grafico
ggplot(TasaAprendizaje, aes(x = Tratamiento, y = TasaPromedio, colour = factor(Dia), group = Dia)) +
  labs(title = "Tasa de Aprendizaje por Tratamiento",
       x = "Tratamiento",
       y = "Tasa de Aprendizaje Promedio") +
  geom_point() +
  geom_line() +
  theme_bw()

#Defino variable GENERALIZA
Datos <- Datos %>%
  rowwise() %>%
  mutate(Generaliza = if_else(`LIO 24hs` == 1 & `NONA 24 hs` == 1, 1, 0))
Generaliza <- Datos%>%
  group_by(Tratamiento) %>%
  summarise(Generaliza = sum(`Generaliza` == 1, na.rm = TRUE), #iGNORA NA
            Total = n_distinct(N),  
            Porcentaje = (Generaliza / Total) * 100)
ggplot(Generaliza, aes(x = Tratamiento, y = Porcentaje, fill=Tratamiento)) +
  geom_bar(stat = "identity") +
  labs(title = "Porcentaje de Abejorros que GENERALIZARON",
       x = "Tratamiento",
       y = "Porcentaje (%)") +
  scale_fill_manual(values = c("SIN CNA" = "lightblue",
                               "CAF" = "#E69138",
                               "ARG" = "#8E7CC3",
                               "CAF+ARG" = "#C27BA0",
                               "2 CAF" = "orange",
                               "2 ARG" = "purple",
                               "2 CAF+ARG" = "#C27"))+
  theme_minimal()
print(Generaliza)

### Modelado ###
DatosModelo<- Datos_filtrado%>%
  filter(Tratamiento != "SIN OLOR") # Nido != "4"
MMixtoNidoDia <- glmer(Recuerda ~ Tratamiento + (1|Nido) + (1|Dia), data=DatosModelo, family=binomial)

isSingular(MMixtoNidoDia) #Da True, la varianza explicad apor dia ya esta expklicada por optras variables del modelo, no aporta nada y complejiza el modelo. Descartamos esta variable aleatoria.

summary (MMixtoNidoDia) # Confirmo

library(emmeans)
library(ggplot2)
library(dplyr)

# Obtener las medias marginales estimadas con intervalos de confianza
emm <- emmeans(MMixtoNidoDia, ~ Tratamiento, type = "response") %>% as.data.frame()
print (emm)
# Graficar las probabilidades estimadas
ggplot(emm, aes(x = Tratamiento, y = prob, fill = Tratamiento)) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  theme_minimal() +
  labs(y = "Probabilidad de memoria (estimada)", x = "Tratamiento") +
  scale_fill_manual(values = c("SIN CNA" = "lightblue",
                               "CAF" = "#E69138",
                               "ARG" = "#8E7CC3",
                               "CAF+ARG" = "#C27BA0",
                               "2 CAF" = "orange",
                               "2 ARG" = "purple",
                               "2 CAF+ARG" = "#C27")) +
  theme(legend.position = "none")


## Modelo mas de 3 ingestas, sin nido 4
DatosModelo2 <-Datos%>%
  filter(Ingestas >= 3, Nido != "4", Tratamiento != "SIN OLOR")
MMixtoNidoDia2 <- glmer(Recuerda ~ Tratamiento + (1|Nido) + (1|Dia), data=DatosModelo2, family=binomial)

isSingular(MMixtoNidoDia2) #Da True, la varianza explicad apor dia ya esta expklicada por optras variables del modelo, no aporta nada y complejiza el modelo. Descartamos esta variable aleatoria.

summary (MMixtoNidoDia2) 

# Obtener las medias marginales estimadas con intervalos de confianza
emm2 <- emmeans(MMixtoNidoDia2, ~ Tratamiento, type = "response") %>% as.data.frame()
print(emm2)
# Graficar las probabilidades estimadas
ggplot(emm2, aes(x = Tratamiento, y = prob, fill = Tratamiento)) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  theme_minimal() +
  labs(y = "Probabilidad de memoria (estimada)", x = "Tratamiento") +
  scale_fill_manual(values = c("SIN CNA" = "lightblue",
                               "CAF" = "#E69138",
                               "ARG" = "#8E7CC3",
                               "CAF+ARG" = "#C27BA0",
                               "2 CAF" = "orange",
                               "2 ARG" = "purple",
                               "2 CAF+ARG" = "#C27")) +
  theme(legend.position = "none")



## Modelo mas de 3 ingestas, sin nido 4, sin variable dia
DatosModelo2 <-Datos%>%
  filter(Ingestas >=3, Nido != "4", Tratamiento != "SIN OLOR")
MMixtoNidoDia2 <- glmer(Recuerda ~ Tratamiento + (1|Nido), data=DatosModelo2, family=binomial)

isSingular(MMixtoNidoDia2) #Da True, la varianza explicad apor dia ya esta expklicada por optras variables del modelo, no aporta nada y complejiza el modelo. Descartamos esta variable aleatoria.

summary (MMixtoNidoDia2) 
anova(MMixtoNidoDia2)
# Obtener las medias marginales estimadas con intervalos de confianza
emm2 <- emmeans(MMixtoNidoDia2, ~ Tratamiento, type = "response") %>% as.data.frame()
print(emm2)
# Graficar las probabilidades estimadas
ggplot(emm2, aes(x = Tratamiento, y = prob, fill = Tratamiento)) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  theme_minimal() +
  labs(y = "Probabilidad de memoria (estimada)", x = "Tratamiento") +
  scale_fill_manual(values = c("SIN CNA" = "lightblue",
                               "CAF" = "#E69138",
                               "ARG" = "#8E7CC3",
                               "CAF+ARG" = "#C27BA0",
                               "2 CAF" = "orange",
                               "2 ARG" = "purple",
                               "2 CAF+ARG" = "#C27")) +
  theme(legend.position = "none")

confint(emm2)
contrasts <- contrast(emm2, method = "pairwise")
summary(contrasts)

