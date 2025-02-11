install.packages("data.table")
library(data.table)
# Definir la ruta al archivo
archivo <- "E:/Juli/ENFR 2018 - Base usuario.txt" 
# Cargar el archivo con el separador '|'
ENFR <- fread(archivo, sep = "|", stringsAsFactors = FALSE)

# Ver las primeras filas para asegurarte de que se haya cargado correctamente
head(ENFR)

#hago una nueva tabla con las variables de interes

"quintil_uc" #quintil de ingresos

#para saber si hay menores hago total de miembros menos miembros mayores de edad
"cant_componentes" - "miembros_18"
ENFR$cant_menores18 <- ENFR$cant_componentes - ENFR$miembros_18
#Durante los últimos 30 días, ¿notó que alguien fumó en dentro de su casa?
"bita10_01"

#¿Qué edad tenía cuando fumó por primera vez? 
"bita02"

#Actualmente ¿fuma usted cigarrillos… 1 ...todos los días? 2 ...algunos días? 3 ...no fuma?
"bita04"

#Nivel de intruccion del jefe de hogar
# 1 Hasta primario incompleto
# 2 Primario completo y secundario incompleto
# 3 Secundario completo y más
# 4 Educación especial

"nivel_instruccion_agrupado_j"



Tabaco <- ENFR[, c("id","quintil_uc", "cant_componentes", "miembros_18","cant_menores18", "bita10_01", "bita02", "bita04", "nivel_instruccion_agrupado_j")]
# Renombro columnas
colnames(Tabaco)[colnames(Tabaco) %in% c("quintil_uc", "cant_componentes", "miembros_18","cant_menores18", "bita10_01", "bita02", "bita04", "nivel_instruccion_agrupado_j")] <- c("quintil_ing", "integrantes", "mayores18","menores18", "humo_hogar", "edad_primera", "fuma", "instruccion")
Tabaco$quintil_ing<-as.factor(Tabaco$quintil_ing)
Tabaco$instruccion<-as.factor(Tabaco$instruccion)
# Verificamos las primeras filas del nuevo dataset
head(Tabaco)

# Opcional: Guardar el nuevo dataset como un archivo CSV
#write.csv(Tabaco, "Tabaco.csv", row.names = FALSE)


##### Filtrado de datos #####
Tabaco <- subset(Tabaco, instruccion != 4) #saco educacion especial
Tabaco <- subset(Tabaco, humo_hogar != 98) #saco casos en los que el entrevistado no estuvo en su hogar los ultimos 30 dias

########## Exploratorio #######

#CUANTOS DE LOS ENTREVISTADOS FUMAN?
Tabaco$fuma_o_no <- ifelse(Tabaco$fuma %in% c(1, 2), "Fuma", "No Fuma")
library(dplyr)
# Calcular los porcentajes
tabla_fuma <- Tabaco %>%
  group_by(fuma_o_no) %>%
  summarise(cantidad = n()) %>%
  mutate(porcentaje = cantidad / sum(cantidad) * 100)
print (tabla_fuma)

#Cuantos corresponden a cada nivel de ingreso

tabla_fuma <- Tabaco %>%
  group_by(quintil_ing, fuma_o_no) %>%
  summarise(cantidad = n()) %>%
  mutate(porcentaje = cantidad / sum(cantidad) * 100)
tabla_ingresos <- tabla_fuma %>%
  filter(fuma_o_no == "Fuma") %>%
  select(quintil_ing, fuma_o_no, cantidad, porcentaje)
print(tabla_ingresos)
#Cuantos corresponden a cada nivel educativos

tabla_inst <- Tabaco %>%
  group_by(instruccion, fuma_o_no) %>%
  summarise(cantidad = n()) %>%
  mutate(porcentaje = cantidad / sum(cantidad) * 100)
tabla_inst <- tabla_inst %>%
  filter(fuma_o_no == "Fuma") %>%
  select(instruccion, fuma_o_no, cantidad, porcentaje)
print(tabla_inst)

#                ---------                    #
##FUMAN DENTRO  DEL HOGAR/ALGUIEN FUMA DENTRO DEL HOGAR

# Calcular los porcentajes
tabla_humo <- Tabaco %>%
  group_by(humo_hogar) %>%
  summarise(cantidad = n()) %>%
  mutate(porcentaje = cantidad / sum(cantidad) * 100)
print (tabla_humo) #1=si


#Cuantos corresponden a cada nivel de ingreso

tabla_humo_ing <- Tabaco %>%
  group_by(quintil_ing, humo_hogar) %>%
  summarise(cantidad = n()) %>%
  mutate(porcentaje = cantidad / sum(cantidad) * 100)
tabla_humo_ing <- tabla_humo_ing %>%
  filter(humo_hogar == "1") %>%
  select(quintil_ing, humo_hogar, cantidad, porcentaje)
print(tabla_humo_ing )
#Cuantos corresponden a cada nivel educativos

tabla_inst <- Tabaco %>%
  group_by(instruccion,humo_hogar) %>%
  summarise(cantidad = n()) %>%
  mutate(porcentaje = cantidad / sum(cantidad) * 100)
tabla_inst <- tabla_inst %>%
  filter(humo_hogar == "1") %>%
  select(instruccion,  humo_hogar, cantidad, porcentaje)
print(tabla_inst)


#                ---------                    #
##FUMAN DENTRO  DEL HOGAR y hay menores  
# Calcular los porcentajes de las casas con menores, uantas tienen humo de tabaco.

tabla_menores <- Tabaco %>%
  group_by(menores18) %>%
  filter(menores18 != 0)
print (tabla_menores) #solo me quedo con los hogares con menores de edad
  
tabla_humo <- tabla_menores %>%
  group_by(humo_hogar) %>%
  summarise(cantidad = n()) %>%
  mutate(porcentaje = cantidad / sum(cantidad) * 100)
print (tabla_humo) #1=si

tabla_humo_ing <- tabla_menores %>%
  group_by(quintil_ing, humo_hogar) %>%
  summarise(cantidad = n()) %>%
  mutate(porcentaje = cantidad / sum(cantidad) * 100)
tabla_humo_ing <- tabla_humo_ing %>%
  filter(humo_hogar == "1") %>%
  select(quintil_ing, humo_hogar, cantidad, porcentaje)
print(tabla_humo_ing )
#Cuantos corresponden a cada nivel educativos

tabla_inst <- tabla_menores %>%
  group_by(instruccion,humo_hogar) %>%
  summarise(cantidad = n()) %>%
  mutate(porcentaje = cantidad / sum(cantidad) * 100)
tabla_inst <- tabla_inst %>%
  filter(humo_hogar == "1") %>%
  select(instruccion,  humo_hogar, cantidad, porcentaje)
print(tabla_inst)


##### Modelado ####
#Pregunta 1: La exposicion al humo en menores de edad es la misma para todos los niveles educativos y socioeconomicos? 
#Pregunta 2: El nivel educativo o la situacion socioeconomica tiene efecto sobre (% fuma en hogares- %fuma en hogares con menores)

#Modelo nivel educativo y situacion socioeconomica como VE, provincia como VA?, VR= presencia ausencia de humo en hogares con menores de edad.
#VR debe tener el exito=1 y el fracaso = 0 asiqe reasigno los valores de manera uqe las hogares sin humo=0
tabla_menores$humo_hogar <- ifelse(tabla_menores$humo_hogar == 2, 0, tabla_menores$humo_hogar)
# Plantear el modelo de regresión logística
modelo <- glm(humo_hogar ~ quintil_ing+ instruccion, 
              data = tabla_menores, 
              family = binomial(link = "logit"))


summary(modelo)

#supuestos

library(DHARMa)
sim<-simulateResiduals(modelo,n=1000)
testDispersion(sim)#no rechazo supuestos ! :)

library(emmeans)
em_means <- emmeans(modelo , ~ quintil_ing, type = "response") #### Hace las comparaciones
confint(em_means)

contrasts <- contrast(em_means, method = "pairwise")
summary(contrasts)
confint(contrasts)
#### gráfico de comp.
em_means_df <- as.data.frame(em_means) # dataset del emmeans anterior

library(ggplot2)
ggplot(em_means_df, aes(x = quintil_ing, y = prob, ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_point(size = 3) +                       # Puntos para las medias marginales ajustadas
  geom_errorbar(width = 0.2) +                 # Barras de error para los intervalos de confianza
  theme_minimal()


#### lo mismo pero con nivel de intruccion
em_means <- emmeans(modelo , ~ instruccion, type = "response") #### Hace las comparaciones
confint(em_means)

contrasts <- contrast(em_means, method = "pairwise")
summary(contrasts)
confint(contrasts)

#### gráfico de comp.
em_means_df <- as.data.frame(em_means) # dataset del emmeans anterior

ggplot(em_means_df, aes(x = instruccion, y = prob, ymin = asymp.LCL, ymax = asymp.UCL)) +
  geom_point(size = 3) +                       # Puntos para las medias marginales ajustadas
  geom_errorbar(width = 0.2) +                 # Barras de error para los intervalos de confianza
  theme_minimal()





