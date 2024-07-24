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
library('MuMIn')

#Leemos bases de datos meteo
BD_meteo_cadiar <- read_excel('BD_meteo/bd_cadiar_meteo.xlsx')
BD_meteo_finana <- read_excel('BD_meteo/bd_finana_meteo.xlsx')

#Las juntamos
BD_meteo <- rbind(BD_meteo_cadiar, BD_meteo_finana)

#Leemos base de datos quercus
BDQ<-read_excel('BD_ESTADISTICA_pixel - copia.xlsx')

#Juntamos base quercus con meteo
BD <- BDQ %>% inner_join(BD_meteo, by=c('Location', 'Fecha'='FECHA_original'))


library(nlme) #Con este paquete se pueden hacer modelos mixtos pero solo gaussianos y de un factor aleatorio
library(lme4) #Con este podemos hacer con datos generalizados y con mas de un factor aleatorio 
library(lmerTest)

#Primero hacemo un modelo simple con interceptos aleatorios
modelo.simple<-lmer(Flujo_CO2~Tratamiento.x + Tmed + Promedio_EVI + Precip + (1|Fecha) + (1|Location), data=BD)
modelo.simple
anova(modelo.simple)
summary(modelo.simple)

#Segundo hacemos un modelo complejo con interceptos y pendientes aleatorias para cada tratamiento
modelo.complejo<-lmer(Flujo_CO2~Tratamiento.x + Tmed + Promedio_EVI + Precip + (1|Fecha) + (Tratamiento.x|Location) , data=BD) #interceptos y pendientes aleatorias

isSingular(modelo.complejo, tol = 1e-4)

anova(modelo)
summary(modelo.complejo)

anova(modelo.simple, modelo.complejo)






modelo.simple<-lm(Flujo_CO2~Tratamiento.x + Tmed + Promedio_NDVI + Promedio_LSWI + Promedio_NDWI + Promedio_EVI + Precip, data=BD, na.action = na.omit)
summary(modelo.simple)
#Hacemos STEP para ver con que variables nos quedamos
step.modelo<-stepAIC(modelo.simple, direction='both')
summary(step.modelo)


modelo.simple<-lm(Flujo_CO2~Tratamiento.x + Tmed + Promedio_NDVI + Promedio_LSWI + Promedio_NDWI + Precip, data=BD, na.action = na.omit)
summary(modelo.simple)
#Hacemos STEP para ver con que variables nos quedamos
step.modelo<-stepAIC(modelo.simple, direction='both')
summary(step.modelo)


modelo.simple<-lm(Flujo_CO2~Tratamiento.x + Tmed + Promedio_NDVI + Promedio_LSWI + Promedio_NDWI, data=BD, na.action = na.omit)
summary(modelo.simple)
#Hacemos STEP para ver con que variables nos quedamos
step.modelo<-stepAIC(modelo.simple, direction='both')
summary(step.modelo)


modelo.simple<-lmer(Flujo_CO2~Tratamiento.x + Tmed + Promedio_NDVI + Promedio_LSWI + Promedio_NDWI + (1|Fecha) + (1|Location), data=BD, na.action = na.omit)
summary(modelo.simple)






options(na.action = "na.fail") # evita errores
ms1 <- dredge(modelo.simple, extra = "adjR^2", rank = "AICc")
ms1 








