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
detach("package:plyr", unload = TRUE)
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

Datos <- read_excel("Datos abejorros Integrados dosisi simple y doble CONTROL.xlsx",col_names = TRUE,  sheet = "Integración")
summary (Datos$Tratamiento) 
Datos$`LIO 24hs` <- as.numeric(Datos$`LIO 24hs`)
Datos <- Datos %>% mutate(`LIO 24hs` = as.factor(round(`LIO 24hs`, 0)))
Datos$`NONA 24 hs` <- as.numeric(Datos$`NONA 24 hs`)
Datos <- Datos %>% mutate(`NONA 24 hs` = as.factor(round(`NONA 24 hs`, 0)))
Datos <- Datos %>%
  rowwise() %>%
  mutate(Recuerda = if_else(`LIO 24hs` == 1 & `NONA 24 hs` == 0, 1, 0),
         Generaliza = if_else(`LIO 24hs` == 1 & `NONA 24 hs` == 1, 1, 0),
         A = ifelse(rowSums(across(starts_with("E"), ~ . == "A")) > 0, 1, 0))

Datos$Tratamiento<- as.factor(Datos$Tratamiento)
summary(Datos$`LIO 24hs`)

# Orden personalizado de los factores
Datos$Tratamiento <- factor(Datos$Tratamiento, levels = c("SIN OLOR","Control", "ARG", "CAF", "CAF+ARG", "2 ARG", "2 CAF", "2 CAF+ARG"))
Datos$Nido<- factor(Datos$Nido, levels = c("2","4", "5", "6", "7", "8", "9", "11", "12", "13", "14"))
summary (Datos$Tratamiento)
#Defino variable Aprende (A) Recuerda y generaliza
Datos <- Datos %>%
  rowwise() %>%
  mutate(
    Recuerda = if_else(`LIO 24hs` == 1 & `NONA 24 hs` == 0, 1, 0),
    Generaliza = if_else(`LIO 24hs` == 1 & `NONA 24 hs` == 1, 1, 0),
    A = ifelse(sum(c_across(starts_with("E")) == "A", na.rm = TRUE) > 0, 1, 0)
  ) %>%
  ungroup()
#Defino Terciles de peso
Datos$Peso <- as.numeric((Datos$Peso))
summary (Datos$Peso)
Datos$CualiPeso<-ntile(Datos$Peso, 3)
summary(Datos$CualiPeso)


### Descargo el EXCEL con la base de datos ORDENADA y PUIDA!
install.packages("writexl")   # solo la primera vez
library(writexl)

write_xlsx(Datos, "DatosMS.xlsx")
getwd()

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
Generalizacion <- Datos %>%
  mutate(Grupo_Tratamiento = case_when(
    Tratamiento %in% c("CAF", "ARG", "CAF+ARG", "2 CAF", "2 ARG", "2 CAF+ARG") ~ "CAF_ARG",
    Tratamiento %in% c("Control") ~ "Control")) %>%
  filter(!is.na(`LIO 24hs`), !is.na(`NONA 24 hs`)) %>% 
  group_by(Grupo_Tratamiento) %>%
  summarise(
    generalizan = sum(`LIO 24hs` == 1 & `NONA 24 hs` == 1),
    Total = n_distinct(N),
    Porcentaje = (generalizan / Total) * 100,
    .groups = "drop")
Generalizacion # los NA corresponden a SIN OLOR los elimino 
# Graficar las respuestas en porcentaje según el tratamiento
library(ggplot2)
ggplot(Generalizacion, aes(x = Grupo_Tratamiento, y = Porcentaje, fill = Grupo_Tratamiento)) +
  geom_bar(stat = "identity") +
  labs(title = "Porcentaje de Abejorros respondieron a PER en\n función del tratamiento",
       x = "Tratamiento",
       y = "Porcentaje (%)") +
  theme_minimal()

#Modelo
Datos_bin <- Datos %>%
  filter(!is.na(`LIO 24hs`), !is.na(`NONA 24 hs`)) %>%
  mutate(
    Generaliza = if_else(`LIO 24hs` == 1 & `NONA 24 hs` == 1, 1, 0),
    Grupo_Tratamiento = case_when(
      Tratamiento %in% c("CAF", "ARG", "CAF+ARG", "2 CAF", "2 ARG", "2 CAF+ARG") ~ "Con CNA",
      Tratamiento == "Control" ~ "Control"
    ),
    Grupo_Tratamiento = factor(Grupo_Tratamiento,
                               levels = c("Control", "Con CNA"))
  )


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

 #### Calculo da tasa de "mentirosos" estimada #### Intento de GEMINI 

# --- 1. PREPARACIÓN ---
library(boot)
library(dplyr)

# Aseguramos que no haya NAs en las variables críticas para que no falle el cálculo
Datos_Clean <- Datos %>% 
  filter(!is.na(A) & !is.na(Recuerda))

# --- 2. DEFINICIÓN DE LA FUNCIÓN ESTADÍSTICA ---
# Esta función calcula el % de aprendices ocultos basándose en tu lógica:
# (Gente que no expresó pero recordó) / (Tasa de retención de los que sí expresaron)

calc_aprendices_ocultos <- function(data, indices) {
  # 'indices' permite al boot generar las muestras aleatorias
  d <- data[indices, ] 
  
  # A. Calcular Tasa de Retención en el grupo que SÍ expresó aprendizaje (A=1)
  # ¿Cuántos de los que tuvieron A=1 lograron recordar?
  grupo_expresa <- d[d$A == 1, ]
  
  # Evitamos error si en una muestra aleatoria no sale nadie con A=1
  if(nrow(grupo_expresa) == 0) return(NA) 
  
  tasa_retencion <- mean(grupo_expresa$Recuerda)
  
  # Si la tasa es 0 (nadie recordó), no podemos dividir. Devolvemos NA.
  if(tasa_retencion == 0) return(NA)
  
  # B. Contar los "Mentirosos Evidentes" (A=0 pero Recuerda=1)
  # Estos son los que sabemos seguro que aprendieron pero no lo mostraron
  mentirosos_observados <- sum(d$A == 0 & d$Recuerda == 1)
  
  # C. Extrapolación (Tu razonamiento lógico)
  # Si solo vimos a los que recordaron, pero hubo otros que aprendieron, no mostraron y olvidaron:
  total_mentirosos_est <- mentirosos_observados / tasa_retencion
  
  # D. Calcular el porcentaje sobre el TOTAL de abejorros
  # (Total estimados / Total de individuos n) * 100
  porcentaje_final <- (total_mentirosos_est / nrow(d)) * 100
  
  return(porcentaje_final)
}

# --- 3. EJECUCIÓN DEL BOOTSTRAP ---
# R = 2000 es un buen número estándar para publicaciones (puedes subir a 10000 si quieres más precisión)
set.seed(123) # Para que el resultado sea reproducible
resultados_boot <- boot(data = Datos_Clean, statistic = calc_aprendices_ocultos, R = 2000)

# --- 4. RESULTADOS E INTERVALOS ---
print(resultados_boot)

# Calculamos el intervalo de confianza (Recomendado: BCa o Percentil)
intervalo <- boot.ci(resultados_boot, type = c("perc", "bca"))

print(intervalo)

# --- 5. VISUALIZACIÓN RÁPIDA (Opcional) ---
hist(resultados_boot$t, breaks = 30, col = "lightblue", main = "Distribución del % de Aprendices Ocultos", xlab = "% Estimado")
abline(v = resultados_boot$t0, col = "red", lwd = 2) # Tu estimación original

#### Lo mismo pero ahora separando por tratamiento a compara los resultados obtenidos###
# 1. FILTRADO INICIAL
# Quitamos "SIN OLOR" porque ahí la falta de respuesta es real, no latente.
# También aseguramos que no haya NAs.
Datos_Analisis <- Datos %>%
  filter(Tratamiento != "SIN OLOR") %>%
  filter(!is.na(A) & !is.na(Recuerda)) %>%
  droplevels() # Elimina el nivel "SIN OLOR" de la lista de factores

# 2. FUNCIÓN DE BOOTSTRAP (La misma lógica, adaptada para errores)
calc_latentes <- function(data, indices) {
  d <- data[indices, ]
  
  # Grupo que EXPRESA (A=1)
  grupo_expresa <- d[d$A == 1, ]
  
  # Si en esta simulación aleatoria nadie aprendió (pasa en grupos pequeños), devolvemos NA
  if(nrow(grupo_expresa) == 0) return(NA)
  
  # Tasa de Retención del grupo visible
  tasa_retencion <- mean(grupo_expresa$Recuerda)
  
  # Si la retención es 0 (nadie recordó), no podemos extrapolar (división por 0)
  if(tasa_retencion == 0) return(NA)
  
  # Conteo de latentes observados (A=0, R=1)
  latentes_obs <- sum(d$A == 0 & d$Recuerda == 1)
  
  # Estimación Total Latentes = Observados / Tasa
  total_latentes_est <- latentes_obs / tasa_retencion
  
  # Devolver % respecto al total del grupo
  return((total_latentes_est / nrow(d)) * 100)
}

# 3. BUCLE PARA CALCULAR POR TRATAMIENTO
# Creamos una lista vacía para guardar resultados
resultados_lista <- list()
tratamientos_validos <- levels(Datos_Analisis$Tratamiento)

print("Iniciando cálculos por tratamiento (esto puede tardar unos segundos)...")

for(trato in tratamientos_validos) {
  
  # Tomamos solo los datos de ESTE tratamiento
  subset_datos <- Datos_Analisis %>% filter(Tratamiento == trato)
  
  # Verificación de seguridad: Si hay muy pocos datos (ej. <10 abejas), saltamos
  if(nrow(subset_datos) < 10) {
    next
  }
  
  # Ejecutamos Bootstrap (2000 iteraciones)
  set.seed(123)
  boot_res <- boot(data = subset_datos, statistic = calc_latentes, R = 2000)
  
  # Calculamos Intervalo de Confianza (BCa es el mejor, si falla usa Percentil)
  # A veces BCa falla si hay poca varianza, usamos tryCatch para evitar errores
  ci <- tryCatch({
    boot.ci(boot_res, type = "bca")$bca[4:5]
  }, error = function(e) {
    # Si falla BCa, usamos percentil simple
    boot.ci(boot_res, type = "perc")$percent[4:5]
  })
  
  # Guardamos en la lista
  resultados_lista[[trato]] <- data.frame(
    Tratamiento = trato,
    N_Total = nrow(subset_datos),
    N_Aprendieron_Visible = sum(subset_datos$A == 1),
    Porcentaje_Estimado = boot_res$t0, # El valor puntual
    IC_Inferior = ci[1],
    IC_Superior = ci[2]
  )
}

# 4. TABLA FINAL
Tabla_Resultados <- bind_rows(resultados_lista)

# Redondeamos para que se vea bonito
Tabla_Resultados <- Tabla_Resultados %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# 5. VER RESULTADOS
print(Tabla_Resultados)

# Opcional: Graficar para el paper
library(ggplot2)
ggplot(Tabla_Resultados, aes(x = Tratamiento, y = Porcentaje_Estimado)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = IC_Inferior, ymax = IC_Superior), width = 0.2) +
  labs(y = "% Estimado de Aprendices Latentes (No expresan)", 
       title = "Estimación de Aprendizaje No Expresado por Tratamiento") +
  theme_minimal()

