### Librerias####
library(dplyr)
library(ggplot2)
library(readxl)
library(lme4)
library(ggplot2)     
library(dplyr)       
library(car)         
library(glmmTMB)     
library(DHARMa)      
library(performance) 
library(emmeans)     
library(ggeffects)   
library(sjPlot)
 #install.packages("plyr")
library(plyr)
#Defino paleta de colores
colores_trat <- c(
  "Control"     = "#FDDC69",
  "ARG"         = "#78D5D6",
  "CAF"         = "#FA7474",
  "CAF+ARG"     = "#AE80DC",
  "2 ARG"       = "#0079AC",
  "2 CAF"       = "#BE340D",
  "2 CAF+ARG"   = "#5C0492"
)




##### Seteo y filtrado de base de datos #####
Datos <- read_excel("Datos abejorros Integrados dosisi simple y doble 19_3_2025.xlsx",col_names = TRUE,  sheet = "Integración")

summary (Datos$Tratamiento) 

Datos <- Datos %>%
  filter(Tratamiento !="F")%>%
  rowwise() %>%
  mutate(Recuerda = if_else(`LIO 24hs` == 1 & `NONA 24 hs` == 0, 1, 0))

Datos$Tratamiento<- as.factor(Datos$Tratamiento)
# Orden personalizado de los factores
Datos$Tratamiento <- factor(Datos$Tratamiento, levels = c("SIN OLOR","SIN CNA", "ARG", "CAF", "CAF+ARG", "2 ARG", "2 CAF", "2 CAF+ARG"))
Datos$Tratamiento <- revalue(Datos$Tratamiento, c("SIN CNA" = "Control"))
Datos$Nido<- factor(Datos$Nido, levels = c("2","4", "5", "6", "7", "8", "9", "11", "12", "13", "14"))
summary (Datos$Tratamiento)
#Defino variable Aprende (A) Recuerda y generaliza
Datos <- Datos %>%
  rowwise() %>%
  mutate(Recuerda = if_else(`LIO 24hs` == 1 & `NONA 24 hs` == 0, 1, 0),
         Generaliza = if_else(`LIO 24hs` == 1 & `NONA 24 hs` == 1, 1, 0),
         A = ifelse(rowSums(across(starts_with("E"), ~ . == "A")) > 0, 1, 0))
#Defino Terciles de peso
Datos$Peso <- as.numeric((Datos$Peso))
summary (Datos$Peso)
Datos$CualiPeso<-ntile(Datos$Peso, 3)
summary(Datos$CualiPeso)


#Analisis exploratorio####

#Porcentaje que tomó el tratamiento completo
sum(Datos$Ingestas == 6, na.rm = TRUE) / nrow(Datos) * 100
#cuantos individuos corresponden a cada n de ingestas
table(Datos$Ingestas)

#Procentaje de supervivencia
sum(Datos$Muere == 0, na.rm = TRUE) / sum(!is.na(Datos$Muere)) * 100
summary(Datos$Ingestas!=0)
table(Datos$Tratamiento[Datos$Ingestas != 0], Datos$A[Datos$Ingestas != 0])

table(Datos$Tratamiento[Datos$Ingestas != 0 & Datos$Muere != 1], Datos$Recuerda[Datos$Ingestas != 0 & Datos$Muere != 1])
table(Datos$Tratamiento[Datos$Ingestas != 0], Datos$Muere[Datos$Ingestas != 0])


############## ADQUISICION PESO Y TRATAMNIENTO va DIA Y NIDO###################
str(Datos$Peso)

#EXPLORATORIO ADQUISICION VS PESO
AprendPeso <- Datos %>%
  filter(!is.na(A)) %>%  # Ignora los NA en A
  group_by(CualiPeso) %>%
  summarise(
    Aprenden = sum(A == "1"),
    Total = n(),
    Aprendizaje = Aprenden / Total
  ) %>%
  mutate(
    Aprendizaje = round(Aprendizaje * 100, 1),
    CualiPeso = as.factor(CualiPeso)
  )

print(AprendPeso)


                                    ##MODELO
Datos$A <- as.numeric(Datos$A)
Datos_filtcon_Ing <- Datos %>% filter(Ingestas>0, !is.na(A), Tratamiento != "SIN OLOR")
# Ajustar modelo logístico
MAprePeso <-glmer(A ~ as.factor(CualiPeso) + Tratamiento + (1|Nido) + (1|Dia), 
                  data = Datos_filtcon_Ing, family = binomial)
summary(MAprePeso)
Anova(MAprePeso, type = 3)
                            ### MARGINALES PESO  ###
# Estimar medias marginales (en escala de respuesta = porcentaje)
em_means <- emmeans(MAprePeso, ~ CualiPeso, type = "response")
em_means_df <- as.data.frame(em_means)
print(em_means_df)
colnames(em_means_df)[colnames(em_means_df) == "prob"] <- "Aprendizaje"
em_means_df$Aprendizaje <- round(em_means_df$Aprendizaje * 100, 1) # Aprendizaje a porcentaje
em_means_df$CualiPeso <- as.factor(em_means_df$CualiPeso)

# Gráfico
ggplot(em_means_df, aes(x = CualiPeso, y = Aprendizaje)) +
  geom_col(fill = "purple") +
  geom_text(aes(label = paste0(Aprendizaje, "%")), vjust = -0.5, hjust = -0.3, size = 4) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 105)) +
  geom_errorbar(aes(ymin = asymp.LCL * 100, ymax = asymp.UCL * 100), width = 0.2) +
  labs(
x = "Weight (tertile)",
y = "Estimated probability of acquisition",
title = "Estimated probability of acquisition by bumblebee weight"
  ) +
  theme_minimal()

# Comparaciones post hoc entre niveles de Peso con ajuste de Tukey
comparaciones <- pairs(em_means, adjust = "tukey")
print(comparaciones)

                                ### MARGINALES TRATAMIENTO  ###
# Estimo medias marginales por tratamiento (escala de respuesta)
em_means_trat <- emmeans(MAprePeso, ~ Tratamiento, type = "response")
em_means_trat_df <- as.data.frame(em_means_trat)
print(em_means_trat_df)


# GRAFICO
em_means_trat_df$Tratamiento <- as.factor(em_means_trat_df$Tratamiento)

# Gráfico
ggplot(em_means_trat_df, aes(x = Tratamiento, y = prob, color = Tratamiento)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, size = 0.8) +
  scale_color_manual(values = colores_trat) +
  labs(
    title = "Estimated probability of acquisition by treatment",
    x = "Treatment",
    y = "Probability of acquisition (± 95% CI)"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
#CONTRASTES tukey
posthoc_trat <- pairs(em_means_trat, adjust = "tukey")
print(posthoc_trat)

# Contrastes: cada tratamiento comparado solo contra el control
contr_trat_vs_ctrl <- contrast(
  em_means_trat,
  method = "trt.vs.ctrl",
  ref = "Control",   # control
  adjust = "none"    #  ajuste
)

summary(contr_trat_vs_ctrl, infer = TRUE)


############### GENERALIZACION #######
Generalizacion <- Datos_filtrado  %>%
  mutate(Grupo_Tratamiento = case_when(
    Tratamiento %in% c("CAF", "ARG", "CAF+ARG", "2 CAF", "2 ARG", "2 CAF+ARG") ~ "CAF_ARG",
    Tratamiento %in% c("Control") ~ "Control"
  )) %>%
  filter(!is.na(`LIO 24hs`), !is.na(`NONA 24 hs`)) %>% 
  group_by(Grupo_Tratamiento) %>%
  summarise(
    generalizan = sum(`LIO 24hs` == 1 & `NONA 24 hs` == 1),
    Total = n_distinct(N),
    Porcentaje = (generalizan / Total) * 100,
    .groups = "drop"
  )
Generalizacion
# Graficar las respuestas en porcentaje según el tratamiento
library(ggplot2)
ggplot(Generalizacion, aes(x = Grupo_Tratamiento, y = Porcentaje, fill = Grupo_Tratamiento)) +
  geom_bar(stat = "identity") +
  labs(title = "Porcentaje de Abejorros respondieron a PER en\n función del tratamiento",
       x = "Tratamiento",
       y = "Porcentaje (%)") +
  theme_minimal()

#Modelo
Datos_bin <- Datos_filtrado %>%
  filter(!is.na(`LIO 24hs`), !is.na(`NONA 24 hs`)) %>%
  mutate(
    Generaliza = if_else(`LIO 24hs` == 1 & `NONA 24 hs` == 1, 1, 0),
    Grupo_Tratamiento = case_when(
      Tratamiento %in% c("CAF", "ARG", "CAF+ARG", "2 CAF", "2 ARG", "2 CAF+ARG") ~ "Con CNA",
      Tratamiento %in% c("Control") ~ "Control"))


modeloGen <- glm(Generaliza ~ Grupo_Tratamiento, family = binomial, data = Datos_bin)


summary(modeloGen)
Anova(modeloGen)
em_means <- emmeans(modeloGen, ~ Grupo_Tratamiento)  # esta es la forma correcta
contrasts <- contrast(em_means, method = "pairwise")
summary(contrasts, infer = TRUE)

em_means_resp <- summary(em_means, type = "response")

ggplot(em_means_resp, aes(x = Grupo_Tratamiento, y = prob, fill = Grupo_Tratamiento)) +
  geom_col(width = 0.6, alpha = 0.6) +  # Barras de fondo
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +  # Barras de error
  geom_point(size = 3, color = "black") +  # Punto de la media ajustada
  labs(x = "Tratamiento",
       y = "Tasa de generalización") +
  scale_fill_manual(values = c("Con CNA" = "purple",
                               "Control" = "#FFD966")) +
  theme_minimal() +
  theme(
    text = element_text(size = 14))

#no hay dif significativas entre tratamientos, calculo porcentaje promedio de generalizacion con intervalo de confianza
modelo_global <- glm(Generaliza ~ 1, family = binomial, data = Datos_bin)

em_global <- emmeans(modelo_global, ~ 1, type = "response")
em_global

