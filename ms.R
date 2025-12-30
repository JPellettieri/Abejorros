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

summary(Datos)
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


          ### TABLA RESUMEN###
library(dplyr)

# 1. Calculamos la tabla por tratamiento
Tabla_Final <- Datos %>%
  group_by(Tratamiento) %>%
  summarise(
    # n: Total de individuos que iniciaron (asumimos que todos ingirieron al menos 1)
    n_ingieren = n(),
    
    # Adquisición: Variable A == 1
    n_adquisicion = sum(A == 1, na.rm = TRUE),
    perc_adquisicion = (n_adquisicion / n_ingieren) * 100,
    
    # Mortalidad: Usamos tu columna 'Muere'
    n_mueren = sum(Muere == 1, na.rm = TRUE),
    perc_mueren = (n_mueren / n_ingieren) * 100,
    
    # Respuesta 24hs: Los que responden (Recuerda o Generaliza) sobre los que NO murieron
    n_vivos = n_ingieren - n_mueren,
    n_responden_24 = sum(Recuerda == 1 | Generaliza == 1, na.rm = TRUE),
    perc_responden_24 = (n_responden_24 / n_vivos) * 100
  ) %>%
  ungroup()

# 2. Calculamos la fila de TOTAL
Total_Fila <- Tabla_Final %>%
  summarise(
    Tratamiento = "Total",
    n_ingieren = sum(n_ingieren),
    n_adquisicion = sum(n_adquisicion),
    perc_adquisicion = (n_adquisicion / n_ingieren) * 100,
    n_mueren = sum(n_mueren),
    perc_mueren = (n_mueren / n_ingieren) * 100,
    n_vivos = sum(n_vivos),
    n_responden_24 = sum(n_responden_24),
    perc_responden_24 = (n_responden_24 / n_vivos) * 100
  )

# 3. Formateamos para que quede igual a tu pedido: "n (%)"
Tabla_Manuscrito <- bind_rows(Tabla_Final, Total_Fila) %>%
  mutate(
    `Ingieren n` = n_ingieren,
    `Adquisición n (%)` = paste0(n_adquisicion, " (", round(perc_adquisicion, 1), "%)"),
    `Mueren n (%)` = paste0(n_mueren, " (", round(perc_mueren, 1), "%)"),
    `Responden 24h n (%)` = paste0(n_responden_24, " (", round(perc_responden_24, 1), "%)")
  ) %>%
  select(Tratamiento, `Ingieren n`, `Adquisición n (%)`, `Mueren n (%)`, `Responden 24h n (%)`)

# 4. Imprimir resultado
print(Tabla_Manuscrito)

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

 #### Calculo da tasa de "mentirosos" estimada #### 
# 1. FILTRADO INICIAL
# Quitamos "SIN OLOR" porque ahí la falta de respuesta es real, no latente.
# También aseguramos que no haya NAs.
Datos_Analisis <- Datos %>%
  filter(Tratamiento != "SIN OLOR") %>%
  filter(Ingestas>0) %>%
  filter(Muere==0) %>%
  filter(!is.na(A) & !is.na(Recuerda)) %>%
  droplevels() # Elimina el nivel "SIN OLOR" de la lista de factores

# 2. FUNCIÓN DE BOOTSTRAP (La misma lógica, adaptada para errores)
calc_latentes <- function(data, indices) {
  d <- data[indices, ]
  grupo_expresa <- d[d$A == 1, ]  # Grupo que EXPRESA (A=1)
  if(nrow(grupo_expresa) == 0) return(NA)
  tasa_retencion <- mean(grupo_expresa$Recuerda) # Tasa de Retención del grupo visible
  if(tasa_retencion == 0) return(NA)
  latentes_obs <- sum(d$A == 0 & d$Recuerda == 1) # Conteo de latentes observados (A=0, R=1)
  total_latentes_est <- latentes_obs / tasa_retencion   # Estimación Total Latentes = Observados / Tasa
  return((total_latentes_est / nrow(d)) * 100) #Calculo %
}

# 3. BUCLE PARA CALCULAR POR TRATAMIENTO
# Creamos una lista vacía para guardar resultados
resultados_lista <- list()
tratamientos_validos <- levels(Datos_Analisis$Tratamiento)
for(trato in tratamientos_validos) {
  subset_datos <- Datos_Analisis %>% filter(Tratamiento == trato)
  
  # Ejecutamos Bootstrap (2000 iteraciones)
  set.seed(123)
  boot_res <- boot(data = subset_datos, statistic = calc_latentes, R = 2000)
  
  ci <- tryCatch({
    boot.ci(boot_res, type = "bca")$bca[4:5]
  }, error = function(e) {
    boot.ci(boot_res, type = "perc")$percent[4:5] # Si falla BCa, usa percentil simple
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
# Redondeo
Tabla_Resultados <- Tabla_Resultados %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# 5. VER RESULTADOS
print(Tabla_Resultados) # aca son los n con al menos una ingestas y descartando los que murieron en el procedimiento

# #Contrastes
# 1. Definimos los valores del Control (Fila 1 de tu Tabla_Resultados)
# Usamos N=59 y Porcentaje=24.72
n_cont_limpio <- 59
perc_cont_limpio <- 24.72
mentirosos_cont_limpio <- (perc_cont_limpio * n_cont_limpio) / 100

# 2. Función de contraste adaptada a los porcentajes
contraste_final <- function(n_trato, perc_trato, n_cont, ment_cont) {
  # Calculamos cuántos individuos representan ese porcentaje
  ment_trato <- (perc_trato * n_trato) / 100
  
  matriz <- matrix(c(ment_trato, (n_trato - ment_trato),
                     ment_cont, (n_cont - ment_cont)), 
                   nrow = 2, byrow = TRUE)
  
  # Usamos prop.test para obtener el p-valor
  return(prop.test(matriz)$p.value)
}

# 3. Aplicamos a la Tabla_Resultados
# Usamos N_Total y Porcentaje_Estimado que son los nombres que tenés
Tabla_Contrastes_Limpia <- Tabla_Resultados %>%
  filter(Tratamiento != "Control") %>% # Filtramos el control para no compararlo consigo mismo
  rowwise() %>%
  mutate(p_valor = contraste_final(N_Total, Porcentaje_Estimado, n_cont_limpio, mentirosos_cont_limpio)) %>%
  ungroup() %>%
  mutate(Significancia = ifelse(p_valor < 0.05, "*", "n.s."))

# 4. Ver los resultados
print(Tabla_Contrastes_Limpia)


# 1. Función usando Fisher Exact Test
contraste_fisher <- function(n_trato, perc_trato, n_cont, perc_cont) {
  # Reconstruimos la matriz de contingencia [Éxitos, Fracasos]
  exitos_trato <- round((perc_trato * n_trato) / 100)
  exitos_cont <- round((perc_cont * n_cont) / 100)
  
  matriz <- matrix(c(exitos_trato, (n_trato - exitos_trato),
                     exitos_cont, (n_cont - exitos_cont)), 
                   nrow = 2, byrow = TRUE)
  
  # Ejecutamos Fisher (test de dos colas)
  return(fisher.test(matriz)$p.value)
}

# 2. Aplicamos a la Tabla_Resultados (limpia)
# Asegúrate de que el control se llame exactamente "Control" o "SIN CNA" según tu tabla
Tabla_Fisher <- Tabla_Resultados %>%
  filter(Tratamiento != "Control") %>% 
  rowwise() %>%
  mutate(p_valor_fisher = contraste_fisher(N_Total, Porcentaje_Estimado, 59, 24.72)) %>%
  ungroup() %>%
  mutate(Significancia = case_when(
    p_valor_fisher < 0.05 ~ "*",
    p_valor_fisher < 0.1 ~ ".", # Tendencia marginal
    TRUE ~ "n.s."
  ))

print(Tabla_Fisher[, c("Tratamiento", "N_Total", "Porcentaje_Estimado", "p_valor_fisher", "Significancia")])
