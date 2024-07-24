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
library(lmPerm)
library('MuMIn')


#ANOVAS
#Leemos base de datos
BD<-read_excel('BD_ESTADISTICA_pixel_SIN_NUBES.xlsx')

#POR SITIO
BD_canar <- subset(BD, Location == 'canar')
BD_finana <- subset(BD, Location == 'finana')

#Canar
modelo_canar<-aov(Flujo_CO2~Tratamiento.x ,data=BD_canar)
summary(modelo_canar)

#Test de normalidad
shapiro.test(residuals(modelo_canar))
#Homogeneidad de varianzas 
bartlett.test(Flujo_CO2~Tratamiento.x ,data=BD_canar)


#Finana
modelo_finana<-aov(Flujo_CO2~Tratamiento.x ,data=BD_finana)
summary(modelo_finana)

#Test de normalidad
shapiro.test(residuals(modelo_finana))
#Homogeneidad de varianzas 
bartlett.test(Flujo_CO2~Tratamiento.x ,data=BD_finana)

#LOGARITMOS

#Canar
modelo_canar<-aov(Flujo_CO2~Tratamiento.x ,data=BD_canar)
summary(modelo_canar)

#Test de normalidad
shapiro.test(residuals(modelo_canar))
#Homogeneidad de varianzas 
bartlett.test(Flujo_CO2~Tratamiento.x ,data=BD_canar)

#finana
modelo_finana<-aov(Flujo_CO2~Tratamiento.x ,data=BD_finana)
summary(modelo_finana)

#Test de normalidad
shapiro.test(residuals(modelo_finana))
#Homogeneidad de varianzas 
bartlett.test(Flujo_CO2~Tratamiento.x ,data=BD_finana)


#TAMPOCO ASI QUE HACEMOS PERMUTACIONES COMO DIJO JM CONDE

set.seed(38) #La semilla siempre antes de las funciones 
modeloaopv<-aovp(Flujo_CO2~Tratamiento.x, data=BD_canar)
summary(modeloaopv)

set.seed(38) #La semilla siempre antes de las funciones 
modeloaopv<-aovp(Flujo_CO2~Tratamiento.x, data=BD_finana)
summary(modeloaopv)

library(permuco) 


prueeP3<-aovperm(Flujo_CO2~Tratamiento.x, data=BD_canar) 
summary(prueeP3)

prueeP3<-aovperm(Flujo_CO2~Tratamiento.x, data=BD_finana) 
summary(prueeP3)



#//////////////////////////////////////////////////////////


#SATELITES


####ANOVAS####


##NDVI

#Primero hacemos analisis con todos los datos juntos. Anova del flujo con el Tratamiento
modelo<-aov(Promedio_NDVI~Tratamiento_pixel, data=BD_finana)
summary(modelo)

#Test de NORMALIDAD
shapiro.test(residuals(modelo))

#Homogeneidad de varianzas 
bartlett.test(Promedio_NDVI~Tratamiento_pixel ,data=BD_finana)

#LOGARITMOS
#Primero hacemos analisis con todos los datos juntos. Anova del flujo con el Tratamiento
modelo<-lm(log(Promedio_NDVI+1)~Tratamiento_pixel, data=BD)
summary(modelo)

#Test de NORMALIDAD
shapiro.test(residuals(modelo))

#Homogeneidad de varianzas 
bartlett.test(log(Flujo_CO2+1)~Tratamiento_pixel, data=BD_finana)



##EVI##

#Primero hacemos analisis con todos los datos juntos. Anova del flujo con el Tratamiento
modelo<-aov(Promedio_EVI~Tratamiento_pixel, data=BD_finana)
summary(modelo)

#Test de NORMALIDAD
shapiro.test(residuals(modelo))

#Homogeneidad de varianzas 
bartlett.test(Promedio_EVI~Tratamiento_pixel ,data=BD_finana)

#LOGARITMOS

#Primero hacemos analisis con todos los datos juntos. Anova del flujo con el Tratamiento
modelo<-lm(log(Promedio_EVI+1)~Tratamiento_pixel, data=BD)
summary(modelo)

#Test de NORMALIDAD
shapiro.test(residuals(modelo))

#Homogeneidad de varianzas 
bartlett.test(log(Promedio_EVI+1)~Tratamiento_pixel, data=BD_finana)


##LSWI##

#Primero hacemos analisis con todos los datos juntos. Anova del flujo con el Tratamiento
modelo<-aov(Promedio_LSWI~Tratamiento_pixel, data=BD_finana)
summary(modelo)

#Test de NORMALIDAD
shapiro.test(residuals(modelo))

#Homogeneidad de varianzas 
bartlett.test(Promedio_LSWI~Tratamiento_pixel ,data=BD_finana)

#LOGARITMOS

#Primero hacemos analisis con todos los datos juntos. Anova del flujo con el Tratamiento
modelo<-lm(log(Promedio_LSWI+1)~Tratamiento_pixel, data=BD)
summary(modelo)

#Test de NORMALIDAD
shapiro.test(residuals(modelo))

#Homogeneidad de varianzas 
bartlett.test(log(Promedio_LSWI+1)~Tratamiento_pixel, data=BD_finana)



##NDWI##

#Primero hacemos analisis con todos los datos juntos. Anova del flujo con el Tratamiento
modelo<-aov(Promedio_NDWI~Tratamiento_pixel, data=BD_finana)
summary(modelo)

#Test de NORMALIDAD
shapiro.test(residuals(modelo))

#Homogeneidad de varianzas 
bartlett.test(Promedio_NDWI~Tratamiento_pixel ,data=BD_finana)

#LOGARITMOS

#Primero hacemos analisis con todos los datos juntos. Anova del flujo con el Tratamiento
modelo<-lm(log(Promedio_NDWI+1)~Tratamiento_pixel, data=BD)
summary(modelo)

#Test de NORMALIDAD
shapiro.test(residuals(modelo))

#Homogeneidad de varianzas 
bartlett.test(log(Promedio_NDWI+1)~Tratamiento_pixel, data=BD_finana)



###PERMUTACIONES

library(permuco) 

#NDVI
prueeP3<-aovperm(Promedio_NDVI~Tratamiento_pixel, data=BD_canar) 
summary(prueeP3)

prueeP3<-aovperm(Promedio_NDVI~Tratamiento_pixel, data=BD_finana) 
summary(prueeP3)


#EVI
prueeP3<-aovperm(Promedio_EVI~Tratamiento_pixel, data=BD_canar) 
summary(prueeP3)

prueeP3<-aovperm(Promedio_EVI~Tratamiento_pixel, data=BD_finana) 
summary(prueeP3)


#LSWI
prueeP3<-aovperm(Promedio_LSWI~Tratamiento_pixel, data=BD_canar) 
summary(prueeP3)

prueeP3<-aovperm(Promedio_LSWI~Tratamiento_pixel, data=BD_finana) 
summary(prueeP3)


#NDWI
prueeP3<-aovperm(Promedio_NDWI~Tratamiento_pixel, data=BD_canar) 
summary(prueeP3)

prueeP3<-aovperm(Promedio_NDWI~Tratamiento_pixel, data=BD_finana) 
summary(prueeP3)

#Albedo
prueeP3<-aovperm(Promedio_albedo~Tratamiento_pixel, data=BD_canar) 
summary(prueeP3)

prueeP3<-aovperm(Promedio_albedo~Tratamiento_pixel, data=BD_finana) 
summary(prueeP3)





####MODELO####
#Leemos bases de datos meteo
BD_meteo_cadiar <- read_excel('BD_meteo/bd_cadiar_meteo.xlsx')
BD_meteo_finana <- read_excel('BD_meteo/bd_finana_meteo.xlsx')

#Las juntamos
BD_meteo <- rbind(BD_meteo_cadiar, BD_meteo_finana)

#Leemos base de datos quercus
BDQ<-read_excel('BD_ESTADISTICA_pixel_SIN_NUBES.xlsx')

#Juntamos base quercus con meteo
BD <- BDQ %>% inner_join(BD_meteo, by=c('Location', 'Fecha'='FECHA_original'))


#POR SITIO
#CANAR
BD_canar <- subset(BD, Location == 'canar')
BD_canar <- subset(BD_canar, Tratamiento_pixel == 'Bajo copa')


modelo.simple<-lm(Flujo_CO2~Tmed + Precip + Promedio_EVI + Promedio_NDWI + Promedio_albedo , data=BD_canar)
summary(modelo.simple)

options(na.action = "na.fail") # evita errores
ms1 <- dredge(modelo.simple, extra = "adjR^2", rank = "AICc")
ms1 


confset <- get.models(ms1, subset=TRUE)
avgmod <- model.avg(confset)
summary(avgmod)
sw(avgmod)

modelo.depurado<-lm(Flujo_CO2~ Tmed * Precip * Promedio_NDVI * Promedio_NDWI, data=BD_canar)
summary(modelo.depurado)

options(na.action = "na.fail") # evita errores
ms1 <- dredge(modelo.depurado, extra = "adjR^2", rank = "AICc")
ms1 

confset <- get.models(ms1, subset=TRUE)
avgmod <- model.avg(confset)
summary(avgmod)
sw(avgmod)

#FINAL CAÑAR BAJO COPA

modelo.depurado<-lm(Flujo_CO2~Promedio_albedo:Promedio_NDVI:Promedio_NDWI:Tmed + Tmed + Promedio_NDVI + Promedio_NDWI + Promedio_albedo + Promedio_albedo:Promedio_NDVI +Promedio_albedo:Promedio_NDWI, data=BD_canar)
summary(modelo.depurado)

#Test de NORMALIDAD
shapiro.test(residuals(modelo.depurado))

#Test de HOMOCEDASTICIDAD 
library(lmtest)
bptest(modelo.depurado)

#Test de LINEALIDAD 
resettest(modelo.depurado)

#Test de OUTLIERS
outlierTest(modelo.depurado)


#PERMUTACIONES
modelo.depurado<-aovperm(Flujo_CO2~Tratamiento.x + Tmed + Promedio_NDVI + Promedio_EVI + Promedio_LSWI + Promedio_NDWI, data=BD_canar)
summary(modelo.depurado)


#FINANA
BD_finana <- subset(BD, Location == 'finana')
BD_finana <- subset(BD_finana, Tratamiento_pixel == 'Suelo desnudo')


modelo.simple<-lm(Flujo_CO2~Tmed * Promedio_NDVI * Promedio_LSWI * Promedio_albedo , data=BD_finana)
summary(modelo.simple)

options(na.action = "na.fail") # evita errores
ms1 <- dredge(modelo.simple, extra = "adjR^2", rank = "AICc")
ms1


confset <- get.models(ms1, subset=TRUE)
avgmod <- model.avg(confset)
summary(avgmod)
sw(avgmod)

modelo.depurado<-lm(Flujo_CO2~   Tmed * Promedio_NDVI * Promedio_NDWI * Promedio_albedo , data=BD_finana)
summary(modelo.depurado)

options(na.action = "na.fail") # evita errores
ms1 <- dredge(modelo.depurado, extra = "adjR^2", rank = "AICc")
ms1

confset <- get.models(ms1, subset=TRUE)
avgmod <- model.avg(confset)
summary(avgmod)
sw(avgmod)

#Test de NORMALIDAD
shapiro.test(residuals(modelo.depurado))

#Test de HOMOCEDASTICIDAD 
library(lmtest)
bptest(modelo.depurado)

#Test de LINEALIDAD 
resettest(modelo.depurado)

#Test de OUTLIERS
outlierTest(modelo.depurado)

#PERMUTACIONES
modelo.depurado<-aovperm(Flujo_CO2~Tratamiento.x + Tmed + Promedio_NDVI + Promedio_EVI + Promedio_LSWI + Promedio_albedo + Precip, data=BD_canar)
summary(modelo.depurado)



























#EFECTO MEDIDAS REPETIDAS

modelo.simple<-lm(Flujo_CO2~Tratamiento.x + Tmed + Precip + Promedio_NDVI + Promedio_EVI + Promedio_LSWI + Promedio_NDWI + Promedio_albedo , data=BD_canar)
summary(modelo.simple)


modG<-lmer(Flujo_CO2~Tratamiento.x * as.factor(Fecha) + (1|Number_Collar), data=BD_canar)
summary(modG)

modG0<-lmer(Flujo_CO2~Tratamiento.x:as.factor(Fecha) + as.factor(Fecha) + (1|Number_Collar), data=BD_canar)
summary(modG0)

library(predictmeans)
library(lme4)
modG<-lmer(Parasitos ~ Tratamiento*Tiempo + (1 | ind), data = fichero)                modG0<-lmer(Parasitos ~ Tiempo + Tratamiento:Tiempo + (1 | ind), data = fichero)
permlmer(modG0, modG, nperm = 10000, ncore=3, plot=FALSE)

#Primero vemos con tratamiento e interacción y luego sin tratamiento
