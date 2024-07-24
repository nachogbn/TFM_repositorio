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
BD<-read_excel('BD_definitiva.xlsx')

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


####ANALISIS NO PARAMETRICOS####

#TAMPOCO CUMPLE LAS ASUNCIONES ASÍ QUE ANALISIS NO PARAMETRICO
#Hacemos kruskal test y lo representamos

#Primero por Tratamiento
kruskal.test(Flujo_CO2 ~ Tratamiento, data = BD)

pairwise.wilcox.test(BD$Flujo_CO2, BD$Tratamiento, p.adjust.method = "bonf", paired = F)

ggplot(BD, aes(x = Tratamiento, y = Flujo_CO2, fill = Tratamiento))+
  geom_violin()+
  geom_point()+
  ylim(0,22)+
  stat_compare_means(method = "kruskal",label.y =  22)+
  stat_compare_means(method = "wilcox.test", ref.group = "BS", label = "p.signif", label.y = 20)+
  theme_classic()

#Ahora por sitio
kruskal.test(Flujo_CO2 ~ Sitio, data = BD)

pairwise.wilcox.test(BD$Flujo_CO2, BD$Sitio, p.adjust.method = "bonf", paired = F)

ggplot(BD, aes(x = Sitio, y = Flujo_CO2, fill = Sitio))+
  geom_violin()+
  geom_point()+
  ylim(0,22)+
  stat_compare_means(method = "kruskal",label.y =  22)+
  stat_compare_means(method = "wilcox.test", ref.group = "Oak forest", label = "p.signif", label.y = 20)+
  theme_classic()


####VIOLIN PLOT####
colores_tratamiento <- c("palegreen3", "tan3")  

#Ahora Tratamiento con subset por sitio. Empezamos con Portugos.
kruskal.test(Flujo_CO2 ~ Tratamiento, data = BD_portugos)

pairwise.wilcox.test(BD_portugos$Flujo_CO2, BD_portugos$Tratamiento, p.adjust.method = "bonf", paired = F)

ggplot(BD_portugos, aes(x = Tratamiento, y = Flujo_CO2, fill = Tratamiento))+
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


#Cáñar.
kruskal.test(Flujo_CO2 ~ Tratamiento, data = BD_canar)

pairwise.wilcox.test(BD_canar$Flujo_CO2, BD_canar$Tratamiento, p.adjust.method = "bonf", paired = F)

ggplot(BD_canar, aes(x = Tratamiento, y = Flujo_CO2, fill = Tratamiento))+
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


#Finana.
kruskal.test(Flujo_CO2 ~ Tratamiento, data = BD_finana)

pairwise.wilcox.test(BD_finana$Flujo_CO2, BD_finana$Tratamiento, p.adjust.method = "bonf", paired = F)

ggplot(BD_finana, aes(x = Tratamiento, y = Flujo_CO2, fill = Tratamiento))+
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


#Abrucena.
kruskal.test(Flujo_CO2 ~ Tratamiento, data = BD_abrucena)

pairwise.wilcox.test(BD_abrucena$Flujo_CO2, BD_abrucena$Tratamiento, p.adjust.method = "bonf", paired = F)

ggplot(BD_abrucena, aes(x = Tratamiento, y = Flujo_CO2, fill = Tratamiento))+
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



####REUNIÓN JUNIO####

#OBJETIVO: cambiar los violins por violin plots y hacerlo por sitio cada dos semanas

#Viendo los resultados lo voy a hacer por mes también.


BD_portugos <- subset(BD, Sitio == 'Pine forest (Portugos)')
BD_canar <- subset(BD, Sitio == 'Oak forest')
BD_finana <- subset(BD, Sitio == 'Oak forest (Finana)')
BD_abrucena <- subset(BD, Sitio == 'Pine forest (Abrucena)')


##PORTUGOS

#Por semana ORIGINAL VIOLIN PLOT
ggplot(BD_portugos, aes(x = Semana, y = Flujo_CO2, fill = Tratamiento)) +
  geom_violin() +  # Utilizar geom_violin() en lugar de geom_violin()
  geom_point() +
  ylim(0, 8) +
  stat_compare_means(method = "kruskal", label.y = 9) +
  stat_compare_means(method = "wilcox.test", ref.group = "Bajo biomasa", label = "p.signif", label.y = 7.5) +
  scale_x_discrete(labels = BD_portugos$Semana) +
  theme_classic()


##BARRAS
colores_tratamiento <- c("palegreen3", "tan3")  

ggplot(BD_portugos, aes(x = Semana, y = Media_Flujo_CO2, fill = Tratamiento)) +
  geom_col(position = position_dodge(width = 2), width = 1.8) +
  geom_errorbar(aes(ymin = Media_Flujo_CO2 - SD_Flujo_CO2, ymax = Media_Flujo_CO2 + SD_Flujo_CO2),
                position = position_dodge(width = 2), width = 0.8) +
  ylim(0, 8) +
  facet_grid(cols = vars(Semana), scales = "free_x", switch = "x") +
  scale_x_discrete(labels = BD_portugos$Semana) +
  scale_y_continuous(breaks = seq(0, 8, by = 0.5), limits = c(0, 8)) +  # Personalización de la escala en el eje y
  theme_gray() +
  scale_fill_manual(values = colores_tratamiento) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1}))) +
  theme(axis.text = element_text(size = 20),
        strip.text.x = element_text(size = 17),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17))


##CÁÑAR
#Por semana
ggplot(BD_canar, aes(x = Semana, y = Media_Flujo_CO2, fill = Tratamiento)) +
  geom_col(position = position_dodge(width = 2), width = 1.8) +
  geom_errorbar(aes(ymin = Media_Flujo_CO2 - SD_Flujo_CO2, ymax = Media_Flujo_CO2 + SD_Flujo_CO2),
                position = position_dodge(width = 2), width = 0.8) +
  ylim(0, 8) +
  facet_grid(cols = vars(Semana), scales = "free_x", switch = "x") +
  scale_x_discrete(labels = BD_canar$Semana) +
  scale_y_continuous(breaks = seq(0, 8, by = 0.5), limits = c(0, 8)) +  # Personalización de la escala en el eje y
  theme_gray() +
  scale_fill_manual(values = colores_tratamiento) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1}))) +
  theme(axis.text = element_text(size = 20),
        strip.text.x = element_text(size = 17),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17))

##FIÑANA
#Por semana
ggplot(BD_finana, aes(x = Semana, y = Media_Flujo_CO2, fill = Tratamiento)) +
  geom_col(position = position_dodge(width = 2), width = 1.8) +
  geom_errorbar(aes(ymin = Media_Flujo_CO2 - SD_Flujo_CO2, ymax = Media_Flujo_CO2 + SD_Flujo_CO2),
                position = position_dodge(width = 2), width = 0.8) +
  ylim(0, 8) +
  facet_grid(cols = vars(Semana), scales = "free_x", switch = "x") +
  scale_x_discrete(labels = BD_finana$Semana) +
  scale_y_continuous(breaks = seq(0, 8, by = 0.5), limits = c(0, 8)) +  # Personalización de la escala en el eje y
  theme_gray() +
  scale_fill_manual(values = colores_tratamiento) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1}))) +
  theme(axis.text = element_text(size = 20),
        strip.text.x = element_text(size = 17),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17))


##ABRUCENA
#Por semana
ggplot(BD_abrucena, aes(x = Semana, y = Media_Flujo_CO2, fill = Tratamiento)) +
  geom_col(position = position_dodge(width = 2), width = 1.8) +
  geom_errorbar(aes(ymin = Media_Flujo_CO2 - SD_Flujo_CO2, ymax = Media_Flujo_CO2 + SD_Flujo_CO2),
                position = position_dodge(width = 2), width = 0.8) +
  ylim(0, 8) +
  facet_grid(cols = vars(Semana), scales = "free_x", switch = "x") +
  scale_x_discrete(labels = BD_abrucena$Semana) +
  scale_y_continuous(breaks = seq(0, 8, by = 0.5), limits = c(0, 8)) +  # Personalización de la escala en el eje y
  theme_gray() +
  theme_gray() +
  scale_fill_manual(values = colores_tratamiento) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1}))) +
  theme(axis.text = element_text(size = 20),
        strip.text.x = element_text(size = 17),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17))

