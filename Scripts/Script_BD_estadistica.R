#CARGAMOS LIBRERIAS
library('MASS')
library('lmtest')
library('car')
library('Hmisc')
library('psych')
library('readxl')
library('relaimpo')
library('ez')
library('rcompanion')
library('vegan')
library('ggplot2')
library('ggpubr')
library('ggthemes')
library('dplyr')
library('scales')

#Leemos base de datos quercus
BDQ<-read_excel('BD_definitiva_quercus.xlsx')

#Leemos base de datos satelites
BDS<-read_excel('BD_satelite/bd_albedo.xlsx')

#Quitamos las obs, nos molestaran al juntar tablas
BDS <- subset(BDS, select = -c(1))

#Comprobamos que no haya duplicados por mi error humano
duplicates <- duplicated(BDS) | duplicated(BDS, fromLast = TRUE)

BDS <- BDS[!duplicates, ]

duplicates
###

library(lubridate)

#Añadimos un rango de +-5 días a las fechas que tenemos de muestreos in situ para poder hacerlas cocincidir con las de satelite luego

BDQ <- BDQ %>%
  mutate(FechaInicial1 = Fecha - days(1),  # Resta 5 días para obtener la fecha inicial
         FechaInicial2 = Fecha - days(2),
         FechaInicial3 = Fecha - days(3),
         FechaInicial4 = Fecha - days(4),
         FechaInicial5 = Fecha - days(5),
         FechaFinal1 = Fecha + days(1),
         FechaFinal2 = Fecha + days(2),
         FechaFinal3 = Fecha + days(3),
         FechaFinal4 = Fecha + days(4),
         FechaFinal5 = Fecha + days(5)
         )    # Suma 5 días para obtener la fecha final


#Vemos que datos de de satelite se encuentran en ese rango de días

BD_ <- BDQ %>% left_join( BDS, 
                              by=c('Number_Collar', 'Location', 'Fecha'='Fecha'))

BD_FI1 <- BDQ %>% left_join( BDS, 
                                by=c('Number_Collar', 'Location', 'FechaInicial1'='Fecha'))
BD_FI2 <- BDQ %>% left_join( BDS, 
                                by=c('Number_Collar', 'Location', 'FechaInicial2'='Fecha'))
BD_FI3 <- BDQ %>% left_join( BDS, 
                                by=c('Number_Collar', 'Location', 'FechaInicial3'='Fecha'))
BD_FI4 <- BDQ %>% left_join( BDS, 
                                by=c('Number_Collar', 'Location', 'FechaInicial4'='Fecha'))
BD_FI5 <- BDQ %>% left_join( BDS, 
                                by=c('Number_Collar', 'Location', 'FechaInicial5'='Fecha'))


BD_FF1 <- BDQ %>% left_join( BDS, 
                                by=c('Number_Collar', 'Location', 'FechaFinal1'='Fecha'))
BD_FF2 <- BDQ %>% left_join( BDS, 
                                by=c('Number_Collar', 'Location', 'FechaFinal2'='Fecha'))
BD_FF3 <- BDQ %>% left_join( BDS, 
                                by=c('Number_Collar', 'Location', 'FechaFinal3'='Fecha'))
BD_FF4 <- BDQ %>% left_join( BDS, 
                                by=c('Number_Collar', 'Location', 'FechaFinal4'='Fecha'))
BD_FF5 <- BDQ %>% left_join( BDS, 
                                by=c('Number_Collar', 'Location', 'FechaFinal5'='Fecha'))


#Ponemos columnas que han sido fusionadas para poder juntar las BD

BD_FI1$FechaInicial1 = BD_FI1$Fecha
BD_FI2$FechaInicial2 = BD_FI2$Fecha
BD_FI3$FechaInicial3 = BD_FI3$Fecha
BD_FI4$FechaInicial4 = BD_FI4$Fecha
BD_FI5$FechaInicial5 = BD_FI5$Fecha

BD_FF1$FechaFinal1 = BD_FF1$Fecha
BD_FF2$FechaFinal2 = BD_FF2$Fecha
BD_FF3$FechaFinal3 = BD_FF3$Fecha
BD_FF4$FechaFinal4 = BD_FF4$Fecha
BD_FF5$FechaFinal5 = BD_FF5$Fecha

#Juntamos todas las BD en una sola

BD_binded <- rbind(BD_, BD_FI1, BD_FI2, BD_FI3, BD_FI4, BD_FI5, BD_FF1, BD_FF2, BD_FF3, BD_FF4, BD_FF5)

#Sacamos la BD en formato excel

library('openxlsx')
#write.xlsx(BD_binded, "BD_binded_albedo.xlsx", rowNames = FALSE)

#Hacemos la media de los Values de satelite

DF_Promedio_albedo <- BD_binded %>%
  group_by(Obs) %>%
  summarize(Promedio_albedo = mean(Value, na.rm = TRUE))

BD_binded <- left_join(BD_binded, DF_Promedio_albedo,
                                    by = c("Obs"))

#Eliminamos columnas de las fechas iniciales y finales que molestan
BD_binded <- subset(BD_binded, select = -c(17:28, 30, 31))

#Ahora que tenemos el promedio quitamos duplicados

# Conservar solo la última fila de cada valor duplicado en la columna "ID"
BD_binded_NO_DUPLI <- BD_binded %>%
  group_by(Obs, Tipo_pixel) %>%
  slice(n())

#remove rows from data frame with NA values in column 'tipo_pixel'
BD_binded_NO_DUPLI <- BD_binded_NO_DUPLI[!is.na(BD_binded_NO_DUPLI$Tipo_pixel),]


#BD_binded_final <- BD_binded_NO_DUPLI

#Juntar los otros índices cuando los tenga
BD_binded_final <- left_join(BD_binded_final, DF_Promedio_albedo, by = c('Obs'))

#write.xlsx(BD_binded_final, "BD_ESTADISTICA_pixel.xlsx", rowNames = FALSE)
