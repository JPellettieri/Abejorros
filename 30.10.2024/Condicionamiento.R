# El objetivo de este analisis es evaluar como afectas las ditintas variables ambientales en la cantidad de veces que toma la recompenza el abejorro. Siendo 6 el maximo.
#Hipotesis: ses espera que cuando las condiciones sean similares a las de maxima actividad del nido los abejorros se encuentren mas "coperativos" jaja y tomen mas cantidad de veces la recompenza


#Distribucion: Binomial con n=6
#Variable respuesta: Cantidad de veces que toma la recompenza
#Variables explicativas: 
            #Temperatura
            #Humedad
            #Epoca del año
            #Hora de captura

#Es de interes ver cuales de estas variable son mas relevantes para tener en cuenta en futuras investigaciones

###########
#librerías
library(dplyr)
library(ggplot2)
library(readxl)

#Cargo datos
#setwd("F:/Juli/Documents/Abejorros y Abejas/Abejorros")
Datos <- read_excel("Datos prueba.xlsx",col_names = TRUE,  sheet = "Limpio")
Datos$Tratamiento<- as.factor(Datos$Tratamiento)
summary (Datos$Tratamiento) # para ver el N de cada tratamiento

# Contar el número de veces que los abejorros tomaron la recompensa (T) en las exposiciones (E1 a E6) (o aprendieron "A")
Datos <- Datos %>%
  rowwise() %>%
  mutate(Condicionamiento = sum(c_across(E1:E6) == "T" | c_across(E1:E6) == "A"))

Datos$Humedad<-Datos$`Humedad (%)`

######### Analisis exploratorio #############

########     HUMEDAD  ######### 
#Alta humedad se relaciona con proximidad de tormentas se espera menos respuesta.
Condicionamiento_Humedad <- Datos %>%
  group_by(Humedad) %>%
  summarise(Condicionamiento_Promedio = mean(Condicionamiento, na.rm = TRUE), .groups = "drop")

# Graficar el promedio de condicionamiento por nivel de humedad
ggplot(Condicionamiento_Humedad, aes(x =Humedad, y = Condicionamiento_Promedio)) +
  geom_point () +
  labs(title = "Condicionamiento Promedio según Humedad",
       x = "Humedad (%)",
       y = "Condicionamiento Promedio") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Agregar línea de tendencia
  theme_minimal()

#la verdad nada muy claro pero qsy sepues en el modelo con todo junto puede ser importante

#########  Temperatura #######
#hacer analisis exploratorio


############Para hacer el analisis de hora de captura y epoca del año hay q ver de arreglar la base de datos por alguna razon esos datos estan re curseados #################