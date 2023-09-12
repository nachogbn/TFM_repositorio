library(dplyr)
#install.packages('openxlsx')
library('openxlsx')

BD<-read_excel('6_BD_SN_FlujosGEIs_Nacho_R90.xlsx')

# Suponiendo que la columna que contiene las semanas se llama "Semana" en BD_portugos
# Suponiendo que la columna que contiene los tratamientos se llama "Tratamiento" en BD_portugos
# Suponiendo que la columna que contiene los sitios se llama "sitio" en BD_portugos

# Calcular las medias y desviaciones estándar para cada combinación de "Semana", "Tratamiento" y "sitio"
medias_desviaciones <- BD %>%
  group_by(Semana, Tratamiento, Sitio) %>%
  summarise(Media_Flujo_CO2 = mean(Flujo_CO2, na.rm = TRUE),
            SD_Flujo_CO2 = sd(Flujo_CO2, na.rm = TRUE))



# Guardar el dataframe con media y desviación en un archivo Excel
write.xlsx(medias_desviaciones, "media_desviacion_R90.xlsx", rowNames = FALSE)


# Unir las medias y desviaciones al dataframe original por las columnas "Semana", "Tratamiento" y "sitio"
BDcon_media_desviacion <- left_join(BD, medias_desviaciones,
                                    by = c("Semana", "Tratamiento", "Sitio"))