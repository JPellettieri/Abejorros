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
library(dplyr)
library(ggplot2)
library(readxl)

#Cargo datos
#setwd("F:/Juli/Documents/Abejorros y Abejas/Abejorros")
Datos <- read_excel("Datos prueba.xlsx",col_names = TRUE,  sheet = "Limpio")
Datos$Tratamiento<- as.factor(Datos$Tratamiento)
summary (Datos$Tratamiento) # para ver el N de cada tratamiento



########### Filtrado de datos ###############
# Contar el número de veces que los abejorros tomaron la recompensa (T) en las exposiciones (E1 a E6) (o aprendieron "A")
Datos <- Datos %>%
  rowwise() %>%
  mutate(Condicionamiento = sum(c_across(E1:E6) == "T" | c_across(E1:E6) == "A"))

# Filtrar para los que tomaron al menos 3 veces y sobrevivieron
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
  labs(title = "Porcentaje de Abejorros que respondieron 1 en LIO 24hs y 0 en NONA 24hs",
       x = "Tratamiento",
       y = "Porcentaje (%)") +
  theme_minimal()







#-----------------------------------------------#
##########          Generalizacion         ##########
##### Analisis exploratorio ####
# Calcular el porcentaje que responde a ambos olores en el PER según el tratamiento
Generalizacion <- Filt_Cond_Sup_Esp %>%
  mutate(Grupo_Tratamiento = case_when(
    Tratamiento %in% c("SIN OLOR", "SIN CNA") ~ "SA_50%",  # Agrupa "SIN OLOR" y "SIN CNA" juntos ya que en este contexto solo importa que reciben solucion azucarada
    TRUE ~ Tratamiento  # Mantiene el resto de los tratamientos sin cambios, si queremos podemos agurparlos too
  )) %>%
  group_by(Grupo_Tratamiento) %>%
  summarise(
    Generalizacion = sum(`LIO 24hs` == 1 & `NONA 24 hs` == 1),      # Cuenta cuántos tienen LIO 24hs igual a 1
    Total = n_distinct(N),                 # Reemplaza "N" por el nombre correcto de la columna de ID única
    Porcentaje = (Generalizacion / Total) * 100, # Calcula el porcentaje de respuesta
    .groups = "drop"
  )

# Mostrar los resultados
print(Generalizacion)

# Graficar las respuestas en porcentaje según el tratamiento
library(ggplot2)
ggplot(Generalizacion, aes(x = Grupo_Tratamiento, y = Porcentaje)) +
  geom_bar(stat = "identity") +
  labs(title = "Porcentaje de Abejorros que respondieron a LIO y NONA a las 24hs",
       x = "Tratamiento",
       y = "Porcentaje (%)") +
  theme_minimal()











