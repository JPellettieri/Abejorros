
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
#setwd("E:/Juli/Abejorros") #setwd de la compu de Lau
setwd("F:/Juli/Documents/Abejorros y Abejas/Abejorros")
Datos <- read_excel("Datos.xlsx",col_names = TRUE,  sheet = "Limpio")



Datos$Tratamiento<- as.factor(Datos$Tratamiento)
Datos$Nido<- as.factor(Datos$Nido)
summary (Datos$Tratamiento) # para ver el N de cada tratamiento

#Contar el número de veces que los abejorros tomaron la recompensa (T) en las exposiciones (E1 a E6) (o aprendieron "A")
Datos <- Datos %>% 
  rowwise() %>%
  mutate(Condicionamiento = sum(c_across(E1:E6) == "T" | c_across(E1:E6) == "A"))

#Defino variable respuesta, Recuerda
Datos <- Datos %>%
  rowwise() %>%
  mutate(Recuerda = if_else(`LIO 24hs` == 1 & `NONA 24 hs` == 0, 1, 0))

Datos <- Datos %>%
  filter(Condicionamiento > 0, Muere == 0)

# Filtrar para los que tomaron al menos 3 veces y sobrevivieron. "Criterio de experto".
Filt_3 <- Datos %>%
  filter(Condicionamiento >=3, Muere == 0)
summary (Filt_3$Tratamiento)



## Análisis exploratorio de los datos.

##### Cantidad de ingestas

DatosMemoria <- Filt_3 %>%
  filter(Tratamiento != "SIN OLOR", Nido!="4")
Respuesta_general <- DatosMemoria %>%
  group_by(Tratamiento,Condicionamiento) %>%
  summarise(
    Responden = sum(`LIO 24hs` == 1 & `NONA 24 hs` == 0),  	# Cuenta cuántos tienen LIO 24hs igual a 1
    Total = n_distinct(N),             	# Reemplaza "N" por el nombre correcto de la columna de ID única
    Porcentaje = (Responden / Total), # Calcula el porcentaje de respuesta
    .groups = "drop")
###Gráfico de % Resp vs canti de ingestas, discriminando por tratamiento
ggplot(Respuesta_general, aes(as.numeric(Condicionamiento), Porcentaje, colour = Tratamiento)) +
  geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) + 
  labs(title = "Porcentaje de respuesta a las 24hs en función\n de la ingesta discriminado por tratamiento",
       x = "Cantidad de ingestas",
       y = "Porcentaje de respuesta al PER 24hs") +
  scale_color_manual(values = c("SIN CNA" = "#FFD966",
                                "CAF" = "#E69138",
                                "ARG" = "#8E7CC3",
                                "CAF+ARG" = "#C27BA0")) +
  theme(text = element_text(size = 14))


#*Figura 1: Porcentaje de respuesta al PER a las 24hs en función de la cantidad de ingesta y discriminando por tratamiento.*

summary(DatosMemoria[,4:5])
with(DatosMemoria,table(Tratamiento, Condicionamiento))


#*Tabla 2: Cantidad de individuos que se sometieron a cada tratamiento y que ingirieron distintas veces la recompensa en el entrenamiento.*
  
Ingestas <- Datos %>%
  group_by(Condicionamiento, Tratamiento) %>%
  summarise(Probabilidad = mean(Recuerda)*100, n = n())
print (Ingestas)


#*Tabla 3: Porcentaje que respondió al olor condicionado y no al incondicionado (respuesta al PER) durante la evaluación en función de la cantidad de ingestas durante el entrenamiento y número de abejorros para cada uno de estos grupos*
  
# Graficar la probabilidad
ggplot(Ingestas, aes(x = as.factor(Condicionamiento), y = Probabilidad)) +
  geom_col(fill = "orange") +
  labs(title = "Probabilidad de recordar el olor a las 24hs en función\n de la cantidad de ingestas en el entrenamiento.",
       x = "Cantidad de ingestas",
       y = "Probabilidad de responder al PER (%)") +
  theme_minimal()  #corregir para que tomas=2 también lo grafique


#*Figura 2: Porcentaje que respondió al PER en función de la cantidad de
#ingestas durante el entrenamiento.*
  

##### Tratamiento

Respuesta <- Filt_3 %>%
  group_by(Tratamiento) %>%
  summarise(
    Responden = sum(`LIO 24hs` == 1 & `NONA 24 hs` == 0),      # Cuenta cuántos tienen LIO 24hs igual a 1
    Total = n_distinct(N),                 # Reemplaza "N" por el nombre correcto de la columna de ID única
    Porcentaje = (Responden / Total) * 100, # Calcula el porcentaje de respuesta
    .groups = "drop"
  )
Respuesta


Generalizacion <- Filt_3 %>%
  group_by(Tratamiento) %>%
  summarise(
    generalizan = sum(`LIO 24hs` == 1 & `NONA 24 hs` == 1),      # Cuenta cuántos tienen LIO 24hs igual a 1
    Total = n_distinct(N),                 # Reemplaza "N" por el nombre correcto de la columna de ID única
    Porcentaje = (generalizan / Total) * 100, # Calcula el porcentaje de respuesta
    .groups = "drop"
  )
Generalizacion
# Graficar las respuestas en porcentaje según el tratamiento
library(ggplot2)
ggplot(Respuesta, aes(x = Tratamiento, y = Porcentaje, fill = Tratamiento)) +
  geom_bar(stat = "identity") +
  labs(title = "Porcentaje de Abejorros respondieron a PER en\n función del tratamiento",
       x = "Tratamiento",
       y = "Porcentaje (%)") +
  scale_fill_manual(values = c("SIN OLOR" = "#76A5Af",
                               "SIN CNA" = "#FFD966",
                               "CAF" = "#E69138",
                               "ARG" = "#8E7CC3",
                               "CAF+ARG" = "#C27BA0")) +
  theme_minimal()


#*Figura 3: Porcentaje de abejorros que respondieron a PER en función del
#tratamiento.*
  

#### Análisis de las variables aleatorias


#####Nido:

#Los datos Sin olor no los vamos a incluir en el modelo asique los borro
DatosMemoria <- Filt_3 %>%
  filter(Tratamiento != "SIN OLOR")
#####HAgo spagetti plot para evaluar los distintos nidos #########
table (Datos$Nido,Datos$Tratamiento)# n de cada tratamiento para cada nido


#**Tabla 4: Número total de animales en función del tratamiento para los
#distintos nidos.**

# Calcular la tasa de aprendizaje promedio por tratamiento
TasaAprendizaje <- DatosMemoria %>%
  group_by(Tratamiento, Nido) %>%
  summarise(TasaPromedio = mean(Recuerda, na.rm = TRUE), .groups = "drop")
# Tasas de aprendizaje por Nido
ggplot(TasaAprendizaje, aes(x = Tratamiento, y = TasaPromedio, colour =as.factor(Nido), group = Nido)) +
  labs(title = "Tasa Promedio de Aprendizaje por Tratamiento\n según el nido ",
       x = "Tratamiento",
       y = "Tasa de Aprendizaje Promedio",
       colour= "Nido") +
  geom_point() +
  geom_line() +
  theme_bw()



#*Figura 4: Gráfico de perfiles de la tasa promedio de aprendizaje en
#función del tratamiento para los distintos nidos evaluados.*
  

summary(Datos$Nido) #Cantidad de datos por nido
Nido4<- DatosMemoria%>%
  filter(Nido == "4")
summary(Nido4$Tratamiento)
#Sacamos al nido 4
DatosMemoria<- DatosMemoria%>%
  filter(Nido != "4")


#### Día:


### grafico de perfiles por dia ###
TasaAprendizaje <- DatosMemoria %>%
  group_by(Tratamiento,Dia) %>%
  summarise(TasaPromedio = mean(Recuerda, na.rm = TRUE), .groups = "drop")
print(TasaAprendizaje)
#Grafico
ggplot(TasaAprendizaje, aes(x = Tratamiento, y = TasaPromedio, colour = factor(Dia), group = Dia)) +
  labs(title = "Tasa de Aprendizaje por Tratamiento según\n el día de captura",
       x = "Tratamiento",
       y = "Tasa de Aprendizaje Promedio") +
  geom_point() +
  geom_line() +
  theme_bw()


#Figura 5: Gráfico de perfiles de la tasa promedio de aprendizaje en función del tratamiento para los distintos días evaluados.

## Modelo


DatosMemoria2 <- DatosMemoria
#Defino variable Condicionamiento2 #######
DatosMemoria <- DatosMemoria %>%
  rowwise() %>%
  mutate( Condicionamiento2 = if_else(Condicionamiento < 5, "Bajo", "Alto"),)

MMixto2<- glmer(Recuerda~Tratamiento+Condicionamiento2+(1|Nido), data=DatosMemoria, family=binomial)


MMixtoNido<- glmer(Recuerda~Tratamiento+(1|Nido), data=DatosMemoria, family=binomial)
anova(MMixto2,MMixtoNido)

summary(MMixtoNido) 
Anova(MMixtoNido)



#Tabla 5:  Probabilidad media y su IC, con una confianza del 95%, de mostrar retención de la memoria a las 24hs para cada tratamiento.

em_means <- emmeans(MMixtoNido, ~ Tratamiento, type = "response") #### Hace las comparaciones para modelo mixto diferenciando por trat
confint(em_means)


contrasts <- contrast(em_means, method = "pairwise")
summary(contrasts)
#### gráfico de comp.
em_means_df <- as.data.frame(em_means) # dataset del emmeans anterior

ggplot(em_means_df, aes(x = Tratamiento, y = prob, ymin = asymp.LCL, ymax = asymp.UCL, color = Tratamiento)) +
  geom_point(size = 3) +                       # Puntos para las medias marginales ajustadas
  geom_errorbar(width = 0.2) +                 # Barras de error para los intervalos de confianza
  labs(x = "Tratamiento", y = "Probabilidad de memoria") + # Etiquetas de los ejes
  scale_color_manual(values = c("SIN OLOR" = "#76A5Af",
                                "SIN CNA" = "#E1B701",
                                "CAF" = "#E69138",
                                "ARG" = "#8E7CC3",
                                "CAF+ARG" = "#C27BA0")) +
  theme_minimal()


#Figura 6: Comparación de los valores medios y los IC, con una confianza del 95%,  de la probabilidad de mostrar retención a las 24hs para cada tratamiento.

## Bonferreoni
tratamientos <- c("ARG", "CAF", "CAF+ARG")  # Reemplaza con tus tratamientos
control <- "SIN CNA"  # Nombre del tratamiento de control

# Crear un contraste personalizado para comparar el promedio ponderado de los tratamientos con el control
contraste <- contrast(
  em_means,
  method = list(
    "Con CNA vs Control" = c(
      rep(1 / length(tratamientos), length(tratamientos)),  # Coeficientes para los tratamientos
      -1  # Coeficiente para el control
    )
  ),
  by = NULL
)

# Aplicar la corrección de Bonferroni
OR<-summary(contraste, adjust = "bonferroni")


# Suponiendo que tienes un objeto 'contraste' generado con emmeans::contrast
OR <- summary(contraste, adjust = "bonferroni")
IC <- confint(contraste, adjust = "bonferroni")

# Mostrar resultados
print(OR)
print(IC)

#Tabla 5:  Contraste a priori por Bonferroni entre grupo “Con CNA” o “Promedio” (que agrupa “CAF”, “ARG” y “CAF+ARG”) y grupo “Control” (“Sin CNA”).



# Anexo

### A. 1: 


print(A1)


#Anexo 1: Porcentaje de abejorros que respondieron mediante PER a las 24hs dentro de cada tratamiento,.

### A. 2:


#### MODELO CON DIA Y NIDO COMO VA ####
MMixtoNidoDia <- glmer(Recuerda ~ Tratamiento + (1|Nido) + (1|Dia), data=DatosMemoria, family=binomial)

isSingular(MMixtoNidoDia) #Da True, la varianza explicad apor dia ya esta expklicada por optras variables del modelo, no aporta nada y complejiza el modelo. Descartamos esta variable aleatoria.

summary (MMixtoNidoDia) # Confirmo


#A.2: Al ajustar el modelo con las variables aleatorias Día y Nido, se observa el mensaje boundary(Singular), que indica problemas de singularidad en el modelo. Esto quiere decir que alguna de las variables aleatorias no aporta suficiente variabilidad y, por lo tanto, no contribuye de manera significativa al modelo. Esto fue confirmado al ejecutar isSingular(MMixtoNidoDia) devolviendonos TRUE. Además, al correr el summary del modelo (Anexo 2), observamos que la varianza de la variable aleatoria Día es muy baja, especialmente en comparación con la varianza de la variable Nido.

### A.3

#Supuestos
simm2 <- simulateResiduals(fittedMod = MMixto2)
plot(simm2)



#A3.1: Prueba de ajuste a una distribución uniforme usando un QQplot de residuos re-escalados (izquierda) y evaluación de desviación de la uniformidad y homogeneidad de varianza (derecha) utilizando el paquete DHARMa.

#En el panel izquierdo, se presenta un gráfico QQ de los residuos, donde se observa la distribución de los residuos simulados comparada con la distribución teórica esperada (línea roja). Los resultados de varias pruebas de ajuste también se incluyen en este panel: el test de Kolmogorov-Smirnov (KS test) muestra un valor de p = 0.06095, indicando que no hay desviaciones significativas de la normalidad; el test de dispersión tiene un valor de p = 0.376, lo que sugiere que no hay desviación significativa en la dispersión; y el test de outliers (valor de p = 1) indica que no se encontraron valores atípicos significativos en los residuos.

#En el panel derecho, se muestra un gráfico (boxplot) que evalúa la homogeneidad de los residuos a través de los grupos predictores. El test de Levene, indicado en el gráfico, muestra que no hay diferencias significativas en la varianza de los residuos entre grupos, sugiriendo homogeneidad de varianza. En general, los resultados de ambas gráficas indican que los residuos cumplen con los supuestos de normalidad, homogeneidad y ausencia de outliers, lo que respalda la validez del modelo ajustado.

### A.4: Criterio de selección de modelo


#A.4: AIC contrastando modelo mixto con o sin “Ingestas” como variable fija.

### A.5: Criterio de selección de modelo

#A5.1

#Anexo 5.1: Summary del modelo mixto seleccionado. 

#A5.2

#Anexo 5.2: Test Global mediante Chi Sq. del modelo seleccionado. No se obtiene una diferencia significativa (Chi Sq Test = 0.09525) .