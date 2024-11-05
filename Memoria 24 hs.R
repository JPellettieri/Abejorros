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
install.packages("dplyr")
install.packages("ggplot2")
install.packages("car")
install.packages("glmmTMB")
install.packages("DHARMa")
install.packages("performance")
install.packages("emmeans")
install.packages("ggeffects")
install.packages("sjPlot")

# Paquete para leer archivos Excel
install.packages("readxl")
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



########### Para el modelo ##############
#Los datos Sin olor no los vamos a incluir en el modelo asique los borro
DatosMemoria <- Filt_Cond_Sup_Esp %>%
  filter(Tratamiento != "SIN OLOR")

#Defino variable respuesta, Recuerda
DatosMemoria <- DatosMemoria %>%
  rowwise() %>%
  mutate(Recuerda = if_else(`LIO 24hs` == 1 & `NONA 24 hs` == 0, 1, 0))

######Planteo modelo Mixto con Nido variable aleatoria #######

MMixto<- glmer(Recuerda~Tratamiento+(1|Nido), data=DatosMemoria, family=binomial) 

## Supuestos
simm1 <- simulateResiduals(fittedMod = MMixto) # , refit = T 
plot(simm1)
plotResiduals(simm1, DatosMemoria$Tratamiento)

testDispersion(simm1) # dio re lindo el DHARMA

### prueba de shapiro
shapiro.test(intercept_efectos) #no rechazo normalidad

#Veo si el efecto del tratamiento es significativo
summary(MMixto) #no da sig

#Emmeans para comparar entre tratamientos
#Posibles graficos para ver tendencias


#---------------------------------# Modelo MArginal












