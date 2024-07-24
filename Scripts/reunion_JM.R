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

#Leemos base de datos
BD<-read_excel('BD_ESTADISTICA_pixel - copia.xlsx')

####ANOVAS####

#Primero hacemos analisis con todos los datos juntos. Anova del flujo con el Tratamiento
modelo<-lm(Flujo_CO2~Tratamiento.x ,data=BD)
summary(modelo)

#Test de NORMALIDAD
shapiro.test(residuals(modelo))

#Homogeneidad de varianzas 
bartlett.test(Flujo_CO2~Tratamiento.x ,data=BD)

#No cumple normalidad ni homogeneidad


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









#TAMPOCO CUMPLE LAS ASUNCIONES ASÍ QUE ANALISIS NO PARAMETRICO
kruskal.test(Flujo_CO2 ~ Tratamiento.x, data = BD_canar)

pairwise.wilcox.test(BD_canar$Flujo_CO2, BD_canar$Tratamiento.x, p.adjust.method = "bonf", paired = F)


#Vamos a respresentar los analisis no parametricos por tratamientos con violin plots
colores_tratamiento <- c("palegreen3", "tan3")  

ggplot(BD_canar, aes(x = Tratamiento.x, y = Flujo_CO2, fill = Tratamiento.x))+
  geom_violin()+
  geom_point()+
  ylim(0,15)+
  stat_compare_means(method = "kruskal",label.y =  14)+
  stat_compare_means(method = "wilcox.test", ref.group = "Bajo biomasa", label = "p.signif", label.y = 13)+
  theme_gray() +
  scale_fill_manual(values = colores_tratamiento) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1}))) +
  theme(axis.text = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12))


#//////////////////////////////////////////////////////////


#SATELITES


####ANOVAS####

#Primero hacemos analisis con todos los datos juntos. Anova del flujo con el Tratamiento
modelo<-aov(Promedio_NDVI~Tratamiento.x * as.factor(Fecha),data=BD)
summary(modelo)

#Test de NORMALIDAD
shapiro.test(residuals(modelo))

#Homogeneidad de varianzas 
bartlett.test(Flujo_CO2~Tratamiento.x ,data=BD)

#LOGARITMOS

#Primero hacemos analisis con todos los datos juntos. Anova del flujo con el Tratamiento
modelo<-lm(log(Promedio_NDVI+1)~Tratamiento_pixel * as.factor(Fecha),data=BD)
summary(modelo)

#Test de NORMALIDAD
shapiro.test(residuals(modelo))

#Homogeneidad de varianzas 
bartlett.test(Flujo_CO2~Tratamiento.x ,data=BD)




####KRUSKALL WALLIS DE INDICES SATELITALES####

#Leemos base de datos quercus
BD<-read_excel('BD_ESTADISTICA_pixel.xlsx')

#Vamos a respresentar los analisis no parametricos por tratamientos con violin plots
colores_tratamiento <- c("palegreen3", "tan3")  

#Ahora Tratamiento con subset por sitio. Empezamos con Canar
BD$Tratamiento <- paste(BD$Tratamiento_pixel)

BD_canar <- subset(BD, Location == 'canar')

kruskal.test(Promedio_NDVI ~ Tratamiento, data = BD_canar)

pairwise.wilcox.test(BD_canar$Promedio_NDVI, BD_canar$Tratamiento, p.adjust.method = "bonf", paired = F)

gr1 <- ggplot(BD_canar, aes(x = Tratamiento, y = Promedio_NDVI, fill = Tratamiento))+
  geom_violin(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  scale_y_continuous(limits = c(0,0.8), expand = c(0,0))+
  stat_compare_means(method = "kruskal",label.y =  0.76, label.x = 'Suelo desnudo')+
  stat_compare_means(method = "wilcox.test", ref.group = "Bajo copa", label = "p.signif", label.y = 0.72, label.x = 'Suelo desnudo')+
  theme_classic() +
  scale_fill_manual(values = colores_tratamiento) +
  labs(y='NDVI', x= 'Tratamiento') +
  theme(axis.text = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15))

gr1


#REUNION SCRIPTS DE JM

library(permuco) 
prueeP3<-aovperm(Zooplancton ~ toxico*time + Error(ind/time), data=demo1) # tiempo categÃ³rica e ind cada replica con todos sus tiempos (categÃ³rica) 
summary(prueeP3)


library(lmPerm) prueeP<-aovp(Zooplancton ~ toxico*time + Error(factor(ind)), data=demo1) # tiempo categÃ³rica e ind cada replica con todos sus tiempos (categÃ³rica) 
summary(prueeP)

library(lmPerm)
lmp


library(usdm)
vifstep(fichero, th=5)  #fichero con solo variables explicativas, ver cuales son variables redudndantes para dejarlas o no. Esto se usa en un principio antes de hacer el modelo
#se puede subir a 10 si 5 da problemas


library(predictmeans)
library(lme4)
modG<-lmer(Parasitos ~ Tratamiento*Tiempo + (1 | ind), data = fichero)                modG0<-lmer(Parasitos ~ Tiempo + Tratamiento:Tiempo + (1 | ind), data = fichero)
permlmer(modG0, modG, nperm = 10000, ncore=3, plot=FALSE)

#Primero vemos con tratamiento e interacción y luego sin tratamiento

