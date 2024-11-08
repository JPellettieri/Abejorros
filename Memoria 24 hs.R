###################Contexto teorico minimo###############
### Pregunta de investigacion:###
#¿Como afectan los compuestos no azucarados (CNA) Cafeina (CAF), Arginina (ARG) y la combinacion de los dos en la memoria a largo plazo?

#Distibucion: Bernoulli
#Variable respuesta: Respuesta al PER a las 24hs (dicotomica 1/0)
# LIO= Lionanol, el olor enseñado en el condicionamiento
#NONA= nonanal, olor control que no se les enseñó
#Si responde solo a LIO se puede decir que RECORDO LO APRENDIDO si responde a los dos olores se dice que tiene una respuesta GENERALIZADA. Si generaliza o no tambien puede depender del tratamiento esta bueno analizarlo.

#Variables explicativas: 
#De efecto fijo:
#*Tratamiento (5 niveles): 
#Cafeina (CAF)
#Arginina (ARG)
#Arginina y Cafeina (ARG-CAF)
#Olor y recompensa con solucion azucarada sin CNA (SIN CNA)
#Solo solucion azucarada sin olor (SIN OLOR)

#De efecto aleatorio:
#Nido 
#Hay muchas mas que se podrian sumar despues evaluar cuales
#-------------------------------------------------------------#
#librerías
# Instalación de paquetes
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("car")
#install.packages("glmmTMB")
#install.packages("DHARMa")
#install.packages("performance")
#install.packages("emmeans")
#install.packages("ggeffects")
#install.packages("sjPlot")

# Paquete para leer archivos Excel
#install.packages("readxl")
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

#Cargo datos
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

# Filtrar para los que tomaron al menos 3 veces y sobrevivieron. "Criterio de experto".
Filt_Cond_Sup_Esp <- Datos %>%
  filter(Condicionamiento >= 3, Muere == 0)

#Analisis exploratorio cantidad de tomas y memoria a las 24 hs
DatosPorCondicionamiento <- DatosMemoria %>%
  group_by(Condicionamiento) %>%
  summarise(Probabilidad = mean(Recuerda), n = n())
print(DatosPorCondicionamiento)
# Graficar la probabilidad
ggplot(DatosPorCondicionamiento, aes(x = as.factor(Condicionamiento), y = Probabilidad)) +
  geom_col(fill = "skyblue") +
  labs(title = "Probabilidad PER a 24hs en función de el la cantidad de ingestas en Condicionamiento",
       x = "Cantidad de ingestas en Condicionamiento",
       y = "Probabilidad de PER") +
  theme_minimal()


############ Analisis exploratorio de datos ##########
# Calcular el porcentaje que responde al PER según el tratamiento
Respuesta <- Filt_Cond_Sup_Esp %>%
  group_by(Tratamiento) %>%
  summarise(
    Responden = sum(`LIO 24hs` == 1 & `NONA 24 hs` == 0),      # Cuenta cuántos tienen LIO 24hs igual a 1
    Total = n_distinct(N),                 # Reemplaza "N" por el nombre correcto de la columna de ID única
    Porcentaje = (Responden / Total) * 100, # Calcula el porcentaje de respuesta
    .groups = "drop"
  )

# Mostrar los resultados
print(Respuesta)

# Graficar las respuestas en porcentaje según el tratamiento
library(ggplot2)
ggplot(Respuesta, aes(x = Tratamiento, y = Porcentaje)) +
  geom_bar(stat = "identity") +
  labs(title = "Porcentaje de Abejorros respondieron a PER",
       x = "Tratamiento",
       y = "Porcentaje (%)") +
  theme_minimal()




#### Filtrado DATOS MEMORIA #######
#Los datos Sin olor no los vamos a incluir en el modelo asique los borro
DatosMemoria <- Filt_Cond_Sup_Esp %>%
  filter(Tratamiento != "SIN OLOR")

#Defino variable respuesta, Recuerda
DatosMemoria <- DatosMemoria %>%
  rowwise() %>%
  mutate(Recuerda = if_else(`LIO 24hs` == 1 & `NONA 24 hs` == 0, 1, 0))

#####HAgo spagetti plot para evaluar los distintos nidos #########
# Calcular la tasa de aprendizaje promedio por tratamiento
TasaAprendizaje <- DatosMemoria %>%
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

#EL NIDO 4 NO  SIGUE LA TENDENCIA, SON POCOS DATOS Y EN FIN DE TEMPORADAS LO SACAMOS
DatosMemoria <- DatosMemoria %>%
  filter(Nido != "4")


###### Spagetti plot por estacion #######
TasaAprendizaje <- DatosMemoria %>%
  group_by(Tratamiento, Estacion) %>%
  summarise(TasaPromedio = mean(Recuerda, na.rm = TRUE), .groups = "drop")
print(TasaAprendizaje)

# Grafico
ggplot(TasaAprendizaje, aes(x = Tratamiento, y = TasaPromedio, colour = factor(Estacion), group = Estacion)) +
  labs(title = "Tasa de Aprendizaje por Tratamiento",
       x = "Tratamiento",
       y = "Tasa de Aprendizaje Promedio") +
  geom_point() +
  geom_line() +
  theme_bw()

### Spagetti plot por dia ###
TasaAprendizaje <- DatosMemoria %>%
  group_by(Tratamiento,Día) %>%
  summarise(TasaPromedio = mean(Recuerda, na.rm = TRUE), .groups = "drop")
print(TasaAprendizaje)
#Grafico
ggplot(TasaAprendizaje, aes(x = Tratamiento, y = TasaPromedio, colour = factor(Día), group = Día)) +
  labs(title = "Tasa de Aprendizaje por Tratamiento",
       x = "Tratamiento",
       y = "Tasa de Aprendizaje Promedio") +
  geom_point() +
  geom_line() +
  theme_bw()



########### Para el modelo ##############
#Los datos Sin olor no los vamos a incluir en el modelo asique los borro
DatosMemoria <- Filt_Cond_Sup_Esp %>%
  filter(Tratamiento != "SIN OLOR")

#Defino variable respuesta, Recuerda
DatosMemoria <- DatosMemoria %>%
  rowwise() %>%
  mutate(Recuerda = if_else(`LIO 24hs` == 1 & `NONA 24 hs` == 0, 1, 0))

######Planteo modelo Mixto con Nido variable aleatoria #######

MMixtoNido<- glmer(Recuerda~Tratamiento+(1|Nido), data=DatosMemoria, family=binomial) 

## Supuestos
simm1 <- simulateResiduals(fittedMod = MMixtoNido) # , refit = T 
plot(simm1)
plotResiduals(simm1, DatosMemoria$Tratamiento)

testDispersion(simm1) # dio re lindo el DHARMA

### prueba de shapiro
#intercept_efectos <- alfai$Nido$(Intercept)
#car::qqPlot(intercept_efectos)
#shapiro.test(intercept_efectos) #no rechazo normalidad

#Veo si el efecto del tratamiento es significativo
summary(MMixtoNido) #no da sig
drop1(MMixtoNido)
Anova(MMixtoNido)
em_means <- emmeans(MMixtoNido, ~ Tratamiento, type = "response") #### Hace las comparaciones para modelo mixto diferenciando por trat
contrasts <- contrast(em_means, method = "pairwise")
summary(contrasts)
#### grafico de comp.
em_means_df <- as.data.frame(em_means) # dataset del emmeans anterior

ggplot(em_means_df, aes(x = Tratamiento, y = prob, ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_point(size = 3) +                      # Puntos para las medias marginales ajustadas
  geom_errorbar(width = 0.2) +                # Barras de error para los intervalos de confianza
  labs(x = "Tratamiento", y = "Probabilidad ajustada de memoria") + # Etiquetas de los ejes
  theme_minimal()

## control contra el promedio de los tratamientos . Ponderar por n








############ Modelo mixto con Dia como VA ########
Datos <- read_excel("Datos prueba.xlsx",col_names = TRUE,  sheet = "Limpio")
Datos$Tratamiento<- as.factor(Datos$Tratamiento)

Datos <- Datos %>% # Contar el número de veces que los abejorros tomaron la recompensa (T) en las exposiciones (E1 a E6) (o aprendieron "A")
  rowwise() %>%
  mutate(Condicionamiento = sum(c_across(E1:E6) == "T" | c_across(E1:E6) == "A"))
summary (Datos$Tratamiento) # para ver el N de cada tratamiento
Datos$Nido<- as.factor(Datos$Nido)
Datos$Día<- as.factor(Datos$Día)

#filtros
Filt_Cond_Sup <- Datos %>% #determino cantidad de ingestas
  filter(Condicionamiento >= 3, Muere == 0)

DatosFiltrados <- Filt_Cond_Sup  %>% # Saco nido 4 y sin olor
  filter( Tratamiento != "SIN OLOR") #Nido != "4",

DatosFiltrados <- DatosFiltrados %>%  #Defino variable respuesta, Recuerda
  rowwise() %>%
  mutate(Recuerda = if_else(`LIO 24hs` == 1 & `NONA 24 hs` == 0, 1, 0))

#MODELO CON DIA COMO VA#
MMixtoDia<- glmer(Recuerda~Tratamiento+(1|Día), data=DatosFiltrados, family=binomial) 

###### Supuestos ####
simm1 <- simulateResiduals(fittedMod = MMixtoDia) # , refit = T 
plot(simm1)
plotResiduals(simm1,DatosFiltrados$Tratamiento)

testDispersion(simm1) # dio re lindo el DHARMA

### prueba de shapiro
intercept_efectos <- alfai$Día$'Intercept'
car::qqPlot(intercept_efectos)
shapiro.test(intercept_efectos) #no rechazo normalidad

#Veo si el efecto del tratamiento es significativo
summary(MMixtoDia) #no da sig
drop1(MMixtoDia)
Anova(MMixtoDia)
em_means <- emmeans(MMixtoDia, ~ Tratamiento, type = "response") #### Hace las comparaciones para modelo mixto diferenciando por trat
contrasts <- contrast(em_means, method = "pairwise")
summary(contrasts)
#### grafico de comp.
em_means_df <- as.data.frame(em_means) # dataset del emmeans anterior

ggplot(em_means_df, aes(x = Tratamiento, y = prob, ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_point(size = 3) +                      # Puntos para las medias marginales ajustadas
  geom_errorbar(width = 0.2) +                # Barras de error para los intervalos de confianza
  labs(x = "Tratamiento", y = "Probabilidad ajustada de memoria") + # Etiquetas de los ejes
  theme_minimal()

###### Comparacion a priori con Bonferreoni #####
#Todo contra el control###
# Especificar los tratamientos que deseas promediar y el tratamiento de control
em_means <- emmeans(MMixtoNido, ~ Tratamiento, type = "response")

tratamientos <- c("ARG", "CAF", "CAF+ARG")  # Reemplaza con tus tratamientos
control <- "SIN CNA"  # Nombre del tratamiento de control

# Crear un contraste personalizado para comparar el promedio ponderado de los tratamientos con el control
contraste <- contrast(
  em_means,
  method = list(
    "Promedio vs Control" = c(
      rep(1 / length(tratamientos), length(tratamientos)),  # Coeficientes para los tratamientos
      -1  # Coeficiente para el control
    )
  ),
  by = NULL
)

# Aplicar la corrección de Bonferroni
summary(contraste, adjust = "bonferroni")


#Proximo paso ver que pasa si pongo nido y dia como VA
