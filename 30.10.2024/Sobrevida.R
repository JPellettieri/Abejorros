###################Contexto teorico minimo###############
### Pregunta de investigacion:###
#¿Como afectan los compuestos no azucarados (CNA) en la sobrevida de los abejorros?

#Variable respuesta: Vivo/muerto a las 24 hs (dicotomica 1/0)

#Variables explicativas: 
#De efecto fijo:
#*Tratamiento (2 niveles): 
      #CON CNA: Cafeina (CAF)/ Arginina (ARG) / Arginina y Cafeina (ARG-CAF)
      #SOLUCION AZUCARADA 50% : (SIN CNA/SIN OLOR)

#De efecto aleatorio:
      #Nido 
      #Humedad ambiental?
      #Temperatura?...
      #Hay muchas mas que se podrian sumar despues evaluar cuales
#---------------------------------------------------------------#

###### Filtrado ############

# Contar el número de veces que los abejorros tomaron la recompensa (T) en las exposiciones (E1 a E6) (o aprendieron "A")
Datos <- Datos %>%
  rowwise() %>%
  mutate(Condicionamiento = sum(c_across(E1:E6) == "T" | c_across(E1:E6) == "A"))

# Filtrar para eliminar los registros con menos de 3 veces tomando la recompensa
Datos_filtrado <-Datos%>%
  filter(Condicionamiento >= 3)



###########        Analisis exploratorio    ##########

# Calcular el índice de supervivencia según el tratamiento
Mortalidad <- Datos_filtrado %>%
  mutate(Grupo_Tratamiento = case_when(
    Tratamiento %in% c("CAF", "ARG", "CAF+ARG") ~ "CAF_ARG",  # Agrupar "CAF", "ARG", "CAF+ARG" juntos, si comentan esta linea se puede analizar cada tratamiento por separado.
    Tratamiento %in% c("SIN CNA", "SIN OLOR") ~ "SIN_CNA_OLOR",  # Agrupar "SIN CNA" y "SIN OLOR"
    TRUE ~ Tratamiento  # Mantener los otros tratamientos sin cambios si existieran
  )) %>%
  group_by(Grupo_Tratamiento) %>%
  summarise(Mueren= sum(Muere == 1), 
            Total = n(), 
            Mortalidad = ( Mueren / Total)*100)

# Mostrar la tabla de supervivencia
print(Mortalidad)

# Graficar la supervivencia según el tratamiento
ggplot(Supervivencia, aes(x = Grupo_Tratamiento, y = Mortalidad)) +
  geom_bar(stat = "identity", fill ="orange") +
  labs(title = "Índice de mortalidad según el Tratamiento",
       x = "Tratamiento",
       y = "Mortalidad") +
  theme_minimal()

