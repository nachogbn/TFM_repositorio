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

####ANOVAS####

#Primero hacemos analisis con todos los datos juntos. Anova del flujo con el Tratamiento
modelo<-aov(Flujo_CO2~Tratamiento ,data=BD)
summary(modelo)

#Test de NORMALIDAD
shapiro.test(residuals(modelo))

#Homogeneidad de varianzas 
bartlett.test(Flujo_CO2~Tratamiento ,data=BD)

#No cumple normalidad ni homogeneidad


#POR SITIO

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

#TAMPOCO CUMPLE LAS ASUNCIONES ASÍ QUE ANALISIS NO PARAMETRICO



####ANALISIS NO PARAMETRICOS####
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


####VIOLIN PLOTS####

#Vamos a respresentar los analisis no parametricos por tratamientos con violin plots
 

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
colores_tratamiento <- c("goldenrod3", "goldenrod1") 

kruskal.test(Flujo_CO2 ~ Tratamiento, data = BD_canar)

pairwise.wilcox.test(BD_canar$Flujo_CO2, BD_canar$Microhábitat, p.adjust.method = "bonf", paired = F)

gr1 <- ggplot(BD_canar, aes(x = Microhábitat, y = Flujo_CO2, fill = Microhábitat))+
  geom_violin()+
  geom_point()+
  scale_y_continuous( breaks = seq(0, 13.5, by = 1.5), limits = c(0, 13.5), expand = c(0,0))+
  stat_compare_means(method = "wilcox.test", ref.group = "Bajo copa", label = "p.signif", label.y = 13)+
  theme_gray() +
  scale_fill_manual(values = colores_tratamiento) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})), title ='a)', x="")+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 17, face = "bold.italic", hjust = 0.5))
gr1

#Finana.
colores_tratamiento <- c("royalblue3", "royalblue1") 

kruskal.test(Flujo_CO2 ~ Tratamiento, data = BD_finana)

pairwise.wilcox.test(BD_finana$Flujo_CO2, BD_finana$Microhábitat, p.adjust.method = "bonf", paired = F)

gr2 <- ggplot(BD_finana, aes(x = Microhábitat, y = Flujo_CO2, fill = Microhábitat))+
  geom_violin()+
  geom_point()+
  scale_y_continuous( breaks = seq(0, 13.5, by = 1.5), limits = c(0, 13.5), expand = c(0,0))+
  stat_compare_means(method = "wilcox.test", ref.group = "Bajo copa", label = "p.signif", label.y = 13)+
  scale_fill_manual(values = colores_tratamiento) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})), title ='b)', x="")+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 17, face = "bold.italic", hjust = 0.5))

gr2


(gr1 + gr2) /gr1

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


####GRAFICOS DE BARRAS CON EJE X TEMPORAL####

#Primero necesitamos las medias con sus desviaciones estandard

#Calcular las medias y desviaciones estándar para cada combinación de "Semana", "Tratamiento" y "sitio"
medias_desviaciones <- BD %>%
  group_by(Semana, Tratamiento, Sitio) %>%
  summarise(Media_Flujo_CO2 = mean(Flujo_CO2, na.rm = TRUE),
            SD_Flujo_CO2 = sd(Flujo_CO2, na.rm = TRUE))



#Guardar el dataframe con media y desviación en un archivo Excel
#write.xlsx(medias_desviaciones, "REDONEmedias_BD_definitivaR90.xlsx", rowNames = FALSE)


#PASO FUERA DE R: HE AÑADIDO A ESTE ARCHIVO LAS FECHAS EN FORMATO FECHA MANUALMENTE PARA QUE R LAS LEA CORRECTAMENTE

BD<-read_excel('medias_BD_definitivaR90.xlsx')

BD_portugos <- subset(BD, Sitio == 'Pine forest (Portugos)')
BD_canar <- subset(BD, Sitio == 'Oak forest')
BD_finana <- subset(BD, Sitio == 'Oak forest (Finana)')
BD_abrucena <- subset(BD, Sitio == 'Pine forest (Abrucena)')

colores_tratamiento <- c("palegreen3", "tan3")  

##BARRAS
#Portugos

#Convierte la columna Fecha a formato Date si aún no lo está
BD_portugos$Fecha <- as.Date(BD_portugos$Fecha, format = "%d %b %Y")

#Obtén las fechas únicas de tus datos
fechas_unicas <- unique(BD_portugos$Fecha)

#Grafico
gr8<-ggplot(BD_portugos, aes(x=Fecha, y=Media_Flujo_CO2, fill=Tratamiento)) + 
  geom_bar(stat= 'identity', position = position_dodge(width = 7.6), width = 7) +
  geom_errorbar(aes(ymin = Media_Flujo_CO2 - SD_Flujo_CO2, ymax = Media_Flujo_CO2 + SD_Flujo_CO2),
                position = position_dodge(width = 7.6), width = 4) +
  guides(x = guide_axis(angle = 45))+
  scale_y_continuous(breaks = seq(0, 8, by = 0.5), limits = c(0, 8)) +  # Personalización de la escala en el eje y
  scale_x_date(labels = scales::date_format("%d %b"), 
               breaks = fechas_unicas,  # Usa las fechas únicas como puntos de quiebre
               date_minor_breaks = "1 day") +  # Agrega marcas menores para los días  theme_gray()+
  scale_fill_manual(values = colores_tratamiento) +  # Aplica los colores definidos
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})))+
  theme_classic()+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17))

gr8

#Grafico con regresion
gr8+geom_smooth(aes(fill = Tratamiento), method = "lm", formula = y ~ x, se = TRUE, color= 'black')

#CAÑAR

#Convierte la columna Fecha a formato Date si aún no lo está
BD_canar$Fecha <- as.Date(BD_canar$Fecha, format = "%d %b %Y")

#Obtén las fechas únicas de tus datos
fechas_unicas <- unique(BD_canar$Fecha)
colores_tratamiento <- c("goldenrod3", "goldenrod1")

#Grafico
gr8<-ggplot(BD_canar, aes(x=Fecha, y=Media_Flujo_CO2, fill=Tratamiento)) + 
  geom_bar(stat= 'identity', position = position_dodge(width = 7.6), width = 7) +
  geom_errorbar(aes(ymin = Media_Flujo_CO2 - SD_Flujo_CO2, ymax = Media_Flujo_CO2 + SD_Flujo_CO2),
                position = position_dodge(width = 7.6), width = 4) +
  guides(x = guide_axis(angle = 45))+
  scale_y_continuous(breaks = seq(0, 13.5, by = 1.5), limits = c(0, 9),  expand = c(0, 0)) +  # Personalización de la escala en el eje y
  scale_x_date(labels = scales::date_format("%d %b"), 
               breaks = fechas_unicas,  # Usa las fechas únicas como puntos de quiebre
               date_minor_breaks = "1 day") +  # Agrega marcas menores para los días  theme_gray()+
  scale_fill_manual(values = colores_tratamiento,
                    name = "Microhábitat",
                    labels = c("Bajo copa", "Suelo desnudo")) +  # Aplica los colores definidos
  labs(title ='a)', y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})))+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        axis.title.y = element_text(vjust = +2),
        axis.title.y.right = element_text(vjust = +2),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 17, face = "bold.italic", hjust = 0.5))

gr8

#Grafico con regresion
gr8+geom_smooth(aes(fill = Tratamiento), method = "lm", formula = y ~ x, se = TRUE, color= 'black')



#ABRUCENA

#Convierte la columna Fecha a formato Date si aún no lo está
BD_abrucena$Fecha <- as.Date(BD_abrucena$Fecha, format = "%d %b %Y")

#Obtén las fechas únicas de tus datos
fechas_unicas <- unique(BD_abrucena$Fecha)

#Grafico
gr8<-ggplot(BD_abrucena, aes(x=Fecha, y=Media_Flujo_CO2, fill=Tratamiento)) + 
  geom_bar(stat= 'identity', position = position_dodge(width = 7.6), width = 7) +
  geom_errorbar(aes(ymin = Media_Flujo_CO2 - SD_Flujo_CO2, ymax = Media_Flujo_CO2 + SD_Flujo_CO2),
                position = position_dodge(width = 7.6), width = 4) +
  guides(x = guide_axis(angle = 45))+
  scale_y_continuous(breaks = seq(0, 8, by = 0.5), limits = c(0, 8)) +  # Personalización de la escala en el eje y
  scale_x_date(labels = scales::date_format("%d %b"), 
               breaks = fechas_unicas,  # Usa las fechas únicas como puntos de quiebre
               date_minor_breaks = "1 day") +  # Agrega marcas menores para los días  theme_gray()+
  scale_fill_manual(values = colores_tratamiento) +  # Aplica los colores definidos
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})))+
  theme_classic()+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17))

gr8

#Grafico con regresion
gr8+geom_smooth(aes(fill = Tratamiento), method = "lm", formula = y ~ x, se = TRUE, color= 'black')



#FIÑANA

#Convierte la columna Fecha a formato Date si aún no lo está
BD_finana$Fecha <- as.Date(BD_finana$Fecha, format = "%d %b %Y")

#Obtén las fechas únicas de tus datos
fechas_unicas <- unique(BD_finana$Fecha)


colores_tratamiento <- c("royalblue3", "royalblue1")

#Grafico
gr9<-ggplot(BD_finana, aes(x=Fecha, y=Media_Flujo_CO2, fill=Tratamiento)) + 
  geom_bar(stat= 'identity', position = position_dodge(width = 7.6), width = 7) +
  geom_errorbar(aes(ymin = Media_Flujo_CO2 - SD_Flujo_CO2, ymax = Media_Flujo_CO2 + SD_Flujo_CO2),
                position = position_dodge(width = 7.6), width = 4) +
  guides(x = guide_axis(angle = 45))+
  scale_y_continuous(breaks = seq(0, 9, by = 1.5), limits = c(0, 9),  expand = c(0, 0)) +  # Personalización de la escala en el eje y
  scale_x_date(labels = scales::date_format("%d %b"), 
               breaks = fechas_unicas,  # Usa las fechas únicas como puntos de quiebre
               date_minor_breaks = "1 day") +  # Agrega marcas menores para los días  theme_gray()+
  scale_fill_manual(values = colores_tratamiento,
                    name = "Microhábitat",
                    labels = c("Bajo copa", "Suelo desnudo")) +  # Aplica los colores definidos
  labs(title ='b)', y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})))+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        axis.title.y = element_text(vjust = +2),
        axis.title.y.right = element_text(vjust = +2),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 17, face = "bold.italic", hjust = 0.5))

gr9

library('patchwork')

gr8 / gr9

#Grafico con regresion
gr8+geom_smooth(aes(fill = Tratamiento), method = "lm", formula = y ~ x, se = TRUE, color= 'black')





#//////////////////////////////////////////////////////////////



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



kruskal.test(Promedio_EVI ~ Tratamiento, data = BD_canar)

pairwise.wilcox.test(BD_canar$Promedio_EVI, BD_canar$Tratamiento, p.adjust.method = "bonf", paired = F)

gr2 <- ggplot(BD_canar, aes(x = Tratamiento, y = Promedio_EVI, fill = Tratamiento))+
  geom_violin()+
  geom_point()+
  scale_y_continuous(limits = c(0,0.8), expand = c(0,0))+
  stat_compare_means(method = "kruskal",label.y =  0.76, label.x = 'Suelo desnudo')+
  stat_compare_means(method = "wilcox.test", ref.group = "Bajo copa", label = "p.signif", label.y = 0.72, label.x = 'Suelo desnudo')+
  theme_classic() +
  scale_fill_manual(values = colores_tratamiento) +
  labs(y='EVI', x= 'Tratamiento') +
  theme(axis.text = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15))

gr2



kruskal.test(Promedio_LSWI ~ Tratamiento, data = BD_canar)

pairwise.wilcox.test(BD_canar$Promedio_LSWI, BD_canar$Tratamiento, p.adjust.method = "bonf", paired = F)

gr3 <- ggplot(BD_canar, aes(x = Tratamiento, y = Promedio_LSWI, fill = Tratamiento))+
  geom_violin(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  scale_y_continuous(limits = c(-0.4,0.4), expand = c(0,0))+
  stat_compare_means(method = "kruskal",label.y =  0.36, label.x = 'Suelo desnudo')+
  stat_compare_means(method = "wilcox.test", ref.group = "Bajo copa", label = "p.signif", label.y = 0.32, label.x = 'Suelo desnudo')+
  theme_classic() +
  scale_fill_manual(values = colores_tratamiento) +
  labs(y='LSWI', x= 'Tratamiento') +
  theme(axis.text = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15))

gr3


kruskal.test(Promedio_NDWI ~ Tratamiento, data = BD_canar)

pairwise.wilcox.test(BD_canar$Promedio_NDWI, BD_canar$Tratamiento, p.adjust.method = "bonf", paired = F)

gr4 <- ggplot(BD_canar, aes(x = Tratamiento, y = Promedio_NDWI, fill = Tratamiento))+
  geom_violin(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  scale_y_continuous(limits = c(-0.8,0), expand = c(0,0))+
  stat_compare_means(method = "kruskal",label.y =  -0.04, label.x = 'Suelo desnudo')+
  stat_compare_means(method = "wilcox.test", ref.group = "Bajo copa", label = "p.signif", label.y = -0.08, label.x = 'Suelo desnudo')+
  theme_classic() +
  scale_fill_manual(values = colores_tratamiento) +
  labs(y='NDWI', x= 'Tratamiento') +
  theme(axis.text = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15))

gr4


library('patchwork')

(gr1 + gr2) / (gr3 + gr4)






#Leemos base de datos quercus
BD<-read_excel('BD_ESTADISTICA_pixel.xlsx')

#Vamos a respresentar los analisis no parametricos por tratamientos con violin plots
colores_tratamiento <- c("palegreen3", "tan3")  

#Ahora Tratamiento con subset por sitio. Empezamos con Portugos.
BD$Tratamiento <- paste(BD$Tratamiento_pixel)

BD_finana <- subset(BD, Location == 'finana')

kruskal.test(Promedio_NDVI ~ Tratamiento, data = BD_finana)

pairwise.wilcox.test(BD_finana$Promedio_NDVI, BD_finana$Tratamiento, p.adjust.method = "bonf", paired = F)

gr1 <- ggplot(BD_finana, aes(x = Tratamiento, y = Promedio_NDVI, fill = Tratamiento))+
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



kruskal.test(Promedio_EVI ~ Tratamiento, data = BD_finana)

pairwise.wilcox.test(BD_finana$Promedio_EVI, BD_finana$Tratamiento, p.adjust.method = "bonf", paired = F)

gr2 <- ggplot(BD_finana, aes(x = Tratamiento, y = Promedio_EVI, fill = Tratamiento))+
  geom_violin()+
  geom_point()+
  scale_y_continuous(limits = c(0,0.8), expand = c(0,0))+
  stat_compare_means(method = "kruskal",label.y =  0.76, label.x = 'Suelo desnudo')+
  stat_compare_means(method = "wilcox.test", ref.group = "Bajo copa", label = "p.signif", label.y = 0.72, label.x = 'Suelo desnudo')+
  theme_classic() +
  scale_fill_manual(values = colores_tratamiento) +
  labs(y='EVI', x= 'Tratamiento') +
  theme(axis.text = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15))

gr2



kruskal.test(Promedio_LSWI ~ Tratamiento, data = BD_finana)

pairwise.wilcox.test(BD_finana$Promedio_LSWI, BD_finana$Tratamiento, p.adjust.method = "bonf", paired = F)

gr3 <- ggplot(BD_finana, aes(x = Tratamiento, y = Promedio_LSWI, fill = Tratamiento))+
  geom_violin(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  scale_y_continuous(limits = c(-0.4,0.45), expand = c(0,0))+
  stat_compare_means(method = "kruskal",label.y =  0.42, label.x = 'Suelo desnudo')+
  stat_compare_means(method = "wilcox.test", ref.group = "Bajo copa", label = "p.signif", label.y = 0.38, label.x = 'Suelo desnudo')+
  theme_classic() +
  scale_fill_manual(values = colores_tratamiento) +
  labs(y='LSWI', x= 'Tratamiento') +
  theme(axis.text = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15))

gr3


kruskal.test(Promedio_NDWI ~ Tratamiento, data = BD_finana)

pairwise.wilcox.test(BD_finana$Promedio_NDWI, BD_finana$Tratamiento, p.adjust.method = "bonf", paired = F)

gr4 <- ggplot(BD_finana, aes(x = Tratamiento, y = Promedio_NDWI, fill = Tratamiento))+
  geom_violin(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  scale_y_continuous(limits = c(-0.8,0), expand = c(0,0))+
  stat_compare_means(method = "kruskal",label.y =  -0.04, label.x = 'Suelo desnudo')+
  stat_compare_means(method = "wilcox.test", ref.group = "Bajo copa", label = "p.signif", label.y = -0.08, label.x = 'Suelo desnudo')+
  theme_classic() +
  scale_fill_manual(values = colores_tratamiento) +
  labs(y='NDWI', x= 'Tratamiento') +
  theme(axis.text = element_text(size = 15),
        strip.text.x = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15))

gr4


library('patchwork')

(gr1 + gr2) / (gr3 + gr4)
