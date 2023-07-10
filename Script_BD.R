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

#Leemos base de datos
BD<-read_excel('6_BD_SN_FlujosGEIs_Nacho.xlsx')

####TODOS LOS DATOS####

#Primero hacemos analisis con todos los datos juntos. Anova del flujo con el Tratamiento
modelo<-aov(Flujo_CO2~Tratamiento ,data=BD)
summary(modelo)

#Test de NORMALIDAD
shapiro.test(residuals(modelo))

#Homogeneidad de varianzas 
bartlett.test(Flujo_CO2~Tratamiento ,data=BD)

#No cumple normalidad ni homogeneidad. Hablar con Sergio


####POR SITIO####
BD_portugos <- subset(BD, Sitio == 'Pine forest (Portugos)')
BD_canar <- subset(BD, Sitio == 'Oak forest')
BD_finana <- subset(BD, Sitio == 'Oak forest (Finana)')
BD_abrucena <- subset(BD, Sitio == 'Pine forest (Abrucena)')

#Portugos
modelo_portugos<-aov(Flujo_CO2~Tratamiento ,data=BD_portugos)
summary(modelo_portugos)

#Test de normalidad
shapiro.test(residuals(modelo_portugos))
#Homogeneidad de varianzas 
bartlett.test(Flujo_CO2~Tratamiento ,data=BD_portugos)


#Canar
modelo_canar<-aov(Flujo_CO2~Tratamiento ,data=BD_canar)
summary(modelo_canar)

#Test de normalidad
shapiro.test(residuals(modelo_canar))
#Homogeneidad de varianzas 
bartlett.test(Flujo_CO2~Tratamiento ,data=BD_canar)


#Finana
modelo_finana<-aov(Flujo_CO2~Tratamiento ,data=BD_finana)
summary(modelo_finana)

#Test de normalidad
shapiro.test(residuals(modelo_finana))
#Homogeneidad de varianzas 
bartlett.test(Flujo_CO2~Tratamiento ,data=BD_finana)


#Abrucena
modelo_abrucena<-aov(Flujo_CO2~Tratamiento ,data=BD_abrucena)
summary(modelo_abrucena)

#Test de normalidad
shapiro.test(residuals(modelo_abrucena))
#Homogeneidad de varianzas 
bartlett.test(Flujo_CO2~Tratamiento ,data=BD_abrucena)

####POR GÉNERO####

#Pinus
BD_pinus <- rbind(BD_portugos, BD_abrucena)

modelo_pinus<-aov(Flujo_CO2~Tratamiento ,data=BD_pinus)
summary(modelo_pinus)

#Quercus
BD_quercus <- rbind(BD_canar, BD_finana)

modelo_quercus<-aov(Flujo_CO2~Tratamiento ,data=BD_quercus)
summary(modelo_quercus)



####ANALISIS NO PARAMETRICOS####

#TAMPOCO CUMPLE LAS ASUNCIONES ASÍ QUE ANALISIS NO PARAMETRICO
#Hacemos kruskal test y lo representamos

#Primero por Tratamiento
kruskal.test(Flujo_CO2 ~ Tratamiento, data = BD)

pairwise.wilcox.test(BD$Flujo_CO2, BD$Tratamiento, p.adjust.method = "bonf", paired = F)

ggplot(BD, aes(x = Tratamiento, y = Flujo_CO2, fill = Tratamiento))+
  geom_boxplot()+
  geom_point()+
  ylim(0,22)+
  stat_compare_means(method = "kruskal",label.y =  22)+
  stat_compare_means(method = "wilcox.test", ref.group = "BS", label = "p.signif", label.y = 20)+
  theme_classic()

#Ahora por sitio
kruskal.test(Flujo_CO2 ~ Sitio, data = BD)

pairwise.wilcox.test(BD$Flujo_CO2, BD$Sitio, p.adjust.method = "bonf", paired = F)

ggplot(BD, aes(x = Sitio, y = Flujo_CO2, fill = Sitio))+
  geom_boxplot()+
  geom_point()+
  ylim(0,22)+
  stat_compare_means(method = "kruskal",label.y =  22)+
  stat_compare_means(method = "wilcox.test", ref.group = "Oak forest", label = "p.signif", label.y = 20)+
  theme_classic()


###
#Ahora Tratamiento con subset por sitio. Empezamos con Portugos.
kruskal.test(Flujo_CO2 ~ Tratamiento, data = BD_portugos)

pairwise.wilcox.test(BD_portugos$Flujo_CO2, BD_portugos$Tratamiento, p.adjust.method = "bonf", paired = F)

ggplot(BD_portugos, aes(x = Tratamiento, y = Flujo_CO2, fill = Tratamiento))+
  geom_boxplot()+
  geom_point()+
  ylim(0,22)+
  stat_compare_means(method = "kruskal",label.y =  22)+
  stat_compare_means(method = "wilcox.test", ref.group = "BS", label = "p.signif", label.y = 20)+
  theme_classic()


#Cáñar.
kruskal.test(Flujo_CO2 ~ Tratamiento, data = BD_canar)

pairwise.wilcox.test(BD_canar$Flujo_CO2, BD_canar$Tratamiento, p.adjust.method = "bonf", paired = F)

ggplot(BD_canar, aes(x = Tratamiento, y = Flujo_CO2, fill = Tratamiento))+
  geom_boxplot()+
  geom_point()+
  ylim(0,22)+
  stat_compare_means(method = "kruskal",label.y =  22)+
  stat_compare_means(method = "wilcox.test", ref.group = "BS", label = "p.signif", label.y = 20)+
  theme_classic()


#Finana.
kruskal.test(Flujo_CO2 ~ Tratamiento, data = BD_finana)

pairwise.wilcox.test(BD_finana$Flujo_CO2, BD_finana$Tratamiento, p.adjust.method = "bonf", paired = F)

ggplot(BD_finana, aes(x = Tratamiento, y = Flujo_CO2, fill = Tratamiento))+
  geom_boxplot()+
  geom_point()+
  ylim(0,22)+
  stat_compare_means(method = "kruskal",label.y =  22)+
  stat_compare_means(method = "wilcox.test", ref.group = "BS", label = "p.signif", label.y = 20)+
  theme_classic()


#Abrucena.
kruskal.test(Flujo_CO2 ~ Tratamiento, data = BD_abrucena)

pairwise.wilcox.test(BD_abrucena$Flujo_CO2, BD_abrucena$Tratamiento, p.adjust.method = "bonf", paired = F)

ggplot(BD_abrucena, aes(x = Tratamiento, y = Flujo_CO2, fill = Tratamiento))+
  geom_boxplot()+
  geom_point()+
  ylim(0,22)+
  stat_compare_means(method = "kruskal",label.y =  22)+
  stat_compare_means(method = "wilcox.test", ref.group = "BS", label = "p.signif", label.y = 20)+
  theme_classic()


#Pinus juntos
kruskal.test(Flujo_CO2 ~ Tratamiento, data = BD_pinus)

pairwise.wilcox.test(BD_pinus$Flujo_CO2, BD_pinus$Tratamiento, p.adjust.method = "bonf", paired = F)

ggplot(BD_pinus, aes(x = Tratamiento, y = Flujo_CO2, fill = Tratamiento))+
  geom_boxplot()+
  geom_point()+
  ylim(0,22)+
  stat_compare_means(method = "kruskal",label.y =  22)+
  stat_compare_means(method = "wilcox.test", ref.group = "BS", label = "p.signif", label.y = 20)+
  theme_classic()


#Quercus juntos
kruskal.test(Flujo_CO2 ~ Tratamiento, data = BD_quercus)

pairwise.wilcox.test(BD_quercus$Flujo_CO2, BD_quercus$Tratamiento, p.adjust.method = "bonf", paired = F)

ggplot(BD_quercus, aes(x = Tratamiento, y = Flujo_CO2, fill = Tratamiento))+
  geom_boxplot()+
  geom_point()+
  ylim(0,22)+
  stat_compare_means(method = "kruskal",label.y =  22)+
  stat_compare_means(method = "wilcox.test", ref.group = "BS", label = "p.signif", label.y = 20)+
  theme_classic()





####REUNIÓN JUNIO

#OBJETIVO: cambiar los boxplots por violin plots y hacerlo por sitio cada dos semanas

#Viendo los resultados lo voy a hacer por mes también.


BD_portugos <- subset(BD, Sitio == 'Pine forest (Portugos)')
BD_canar <- subset(BD, Sitio == 'Oak forest')
BD_finana <- subset(BD, Sitio == 'Oak forest (Finana)')
BD_abrucena <- subset(BD, Sitio == 'Pine forest (Abrucena)')


#PORTUGOS

#Por semana
ggplot(BD_portugos, aes(x = Tratamiento, y = Flujo_CO2, fill = Tratamiento)) +
  geom_violin() +  # Utilizar geom_violin() en lugar de geom_boxplot()
  geom_point() +
  ylim(0, 10) +
  stat_compare_means(method = "kruskal", label.y = 9) +
  stat_compare_means(method = "wilcox.test", ref.group = "Bajo biomasa", label = "p.signif", label.y = 7.5) +
  facet_wrap(~ Semana) +
  theme_classic()


#CÁÑAR
ggplot(BD_canar, aes(x = Tratamiento, y = Flujo_CO2, fill = Tratamiento)) +
  geom_violin() +  # Utilizar geom_violin() en lugar de geom_boxplot()
  geom_point() +
  ylim(0, 10) +
  stat_compare_means(method = "kruskal", label.y = 9) +
  stat_compare_means(method = "wilcox.test", ref.group = "Bajo biomasa", label = "p.signif", label.y = 7.5) +
  facet_wrap(~ Semana) +
  theme_classic()

#FIÑANA
ggplot(BD_finana, aes(x = Tratamiento, y = Flujo_CO2, fill = Tratamiento)) +
  geom_violin() +  # Utilizar geom_violin() en lugar de geom_boxplot()
  geom_point() +
  ylim(0, 10) +
  stat_compare_means(method = "kruskal", label.y = 9) +
  stat_compare_means(method = "wilcox.test", ref.group = "Bajo biomasa", label = "p.signif", label.y = 7.5) +
  facet_wrap(~ Semana) +
  theme_classic()

#ABRUCENA
ggplot(BD_abrucena, aes(x = Tratamiento, y = Flujo_CO2, fill = Tratamiento)) +
  geom_violin() +  # Utilizar geom_violin() en lugar de geom_boxplot()
  geom_point() +
  ylim(0, 10) +
  stat_compare_means(method = "kruskal", label.y = 9) +
  stat_compare_means(method = "wilcox.test", ref.group = "Bajo biomasa", label = "p.signif", label.y = 7.5) +
  facet_wrap(~ Semana) +
  theme_classic()


