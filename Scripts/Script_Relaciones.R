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
library('tidyverse')


#Leemos base de datos quercus
BD<-read_excel('BD_ESTADISTICA_pixel_SIN_NUBES.xlsx')

#Junto localizacion y tratamiento en una misma columna para que luego se mas facil graficar

BD$sitiotrat <- paste(BD$Location, BD$Tratamiento.x)
BD

BD$Tratamiento <- paste(BD$Tratamiento.x)

colores_tratamiento <- c("goldenrod3", "goldenrod1", "royalblue3", "royalblue1")  # Define los colores


####GRAFICOS R PEARSON####

#NDVI
cor_coefs <- cor.test(BD$Promedio_NDVI, BD$Flujo_CO2)


gr1 <- ggplot(BD, aes(x=Promedio_NDVI, y=Flujo_CO2)) + 
  geom_point(alpha = 0.7) +
  geom_smooth(method=lm, se=FALSE)+
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 0.81), expand = c(0,0)) + 
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8, by = 1), expand = c(0,0)) +
  guides(linetype = guide_legend(override.aes = list(size = 7))) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})), x = 'NDVI')+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15)) +
  annotate("text", size = 4.5, y = 7, x = 0.7, label = paste0("R: ", round(cor_coefs$estimate, 3))) +
  annotate("text", size = 4.5, y = 6.6, x = 0.743, label = paste0("p-value: ", round(cor_coefs$p.value, 6)))

gr1

#EVI
cor_coefs <- cor.test(BD$Promedio_EVI, BD$Flujo_CO2)

gr2 <- ggplot(BD, aes(x=Promedio_EVI, y=Flujo_CO2)) + 
  geom_point(alpha = 0.7) +
  geom_smooth(method=lm, se=FALSE)+
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 0.505), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8, by = 1), expand = c(0,0)) +
  guides(linetype = guide_legend(override.aes = list(size = 7))) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})), x = 'EVI')+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15)) +
  annotate("text", size = 4.5, y = 7, x = 0.42, label = paste0("R: ", round(cor_coefs$estimate, 3))) +
  annotate("text", size = 4.4, y = 6.6, x = 0.447, label = paste0("p-value: ", round(cor_coefs$p.value, 6)))

gr2

#LSWI
cor_coefs <- cor.test(BD$Promedio_LSWI, BD$Flujo_CO2)

gr3 <- ggplot(BD, aes(x=Promedio_LSWI, y=Flujo_CO2)) + 
  geom_point(alpha = 0.7) +
  geom_smooth(method=lm, se=FALSE)+
  scale_x_continuous(breaks = seq(-0.4, 0.4, by = 0.1), limits = c(-0.4, 0.407), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8, by = 1), expand = c(0,0)) +
  guides(linetype = guide_legend(override.aes = list(size = 7))) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})), x = 'LSWI')+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15)) +
  annotate("text", size = 4.5, y = 7, x = 0.30, label = paste0("R: ", round(cor_coefs$estimate, 3))) +
  annotate("text", size = 4.5, y = 6.6, x = 0.334, label = paste0("p-value: ", round(cor_coefs$p.value, 5)))

gr3


#NDWI

cor_coefs <- cor.test(BD$Promedio_NDWI, BD$Flujo_CO2)

gr4 <- ggplot(BD, aes(x=Promedio_NDWI, y=Flujo_CO2)) + 
  geom_point(alpha = 0.7) +
  geom_smooth(method=lm, se=FALSE)+
  scale_x_continuous(limits = c(-0.707,0), breaks = seq(0,-1, by = -0.1), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8, by = 1), expand = c(0,0)) +
  guides(linetype = guide_legend(override.aes = list(size = 7))) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})), x = 'NDWI')+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15)) +
  annotate("text", size = 4.5, y = 7, x = -0.1, label = paste0("R: ", round(cor_coefs$estimate, 3))) +
  annotate("text", size = 4.5, y = 6.6, x = -0.07, label = paste0("p-value: ", round(cor_coefs$p.value, 5)))

gr4

library('patchwork')

(gr1 + gr2) / (gr3 + gr4)













####INTENTO DE GRAFICOS POR SITIO Y TRATAMIENTO CON LAS REGRESION CON LEYENDA BIEN HECHA####

gr2

gr2 <- ggplot(BD, aes(y=Flujo_CO2, x=Promedio_NDVI, color = sitiotrat, shape = sitiotrat)) + 
  geom_point() +
  geom_smooth(method='lm', se=FALSE, aes(linetype = 'Bajo copa'))+
  geom_smooth(method=lm, se=FALSE, aes(linetype = 'Suelo desnudo'))+
  geom_smooth(method=lm, se=FALSE, aes(linetype = 'Bajo copa'))+
  geom_smooth(method=lm, se=FALSE, aes(linetype = 'Suelo desnudo'))+
  scale_linetype_manual(name = 'Tratamiento',
                        breaks = c('Bajo copa', 'Suelo desnudo'),
                        values = c('Bajo copa' = 'solid', 'Suelo desnudo' = 'dashed'))+
  scale_color_manual(values = colores_tratamiento,
                     name = "Sitio y tratamiento",
                     labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD")) +  # Aplica los colores definidos
  scale_shape_manual(values = c(16, 2, 16, 2),
                     name = "Sitio y tratamiento",
                     labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD"))+
  scale_y_continuous(limits = c(0,8)) +
  guides(linetype = guide_legend(override.aes = list(size = 7))) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})), x = 'NVDI')+
  theme_classic()+
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17))
gr2




####GRAFICO POR SITIO Y TRATAMIENTO####

#NDVI

BD$Tratamiento <- paste(BD$Tratamiento_pixel)

gr5 <- ggplot(BD, aes(y=Flujo_CO2, x=Promedio_NDVI, color = Tipo_pixel, shape = Tipo_pixel)) + 
  geom_point(show.legend = FALSE,alpha = 0.6) +
  geom_smooth(method=lm, se=FALSE, aes(linetype = Tratamiento_pixel), show.legend = FALSE)+
  scale_color_manual(values = colores_tratamiento,
                     name = "Sitio y tratamiento",
                     labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD")) +  # Aplica los colores definidos
  scale_shape_manual(values = c(16, 2, 16, 2),
                        name = "Sitio y tratamiento",
                        labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD"))+
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 0.81), expand = c(0,0)) + 
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8, by = 1), expand = c(0,0)) +
  guides(linetype = guide_legend(override.aes = list(size = 7))) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})), x = 'NDVI')+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15))

gr5

#EVI

gr6 <- ggplot(BD, aes(y=Flujo_CO2, x=Promedio_EVI, color = Tipo_pixel, shape = Tipo_pixel)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method=lm, se=FALSE, aes(linetype = Tratamiento_pixel), show.legend = FALSE)+
  scale_color_manual(values = colores_tratamiento,
                     name = "Sitio y tratamiento",
                     labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD")) +  # Aplica los colores definidos
  scale_shape_manual(values = c(16, 2, 16, 2),
                     name = "Sitio y tratamiento",
                     labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD"))+
  scale_x_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 0.505), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8, by = 1), expand = c(0,0))  +
  guides(linetype = guide_legend(override.aes = list(size = 7))) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})), x = 'EVI')+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15))

gr6

#LSWI
gr7 <- ggplot(BD, aes(y=Flujo_CO2, x=Promedio_LSWI, color = Tipo_pixel, shape = Tipo_pixel)) + 
  geom_point(show.legend = FALSE, alpha = 0.6) +
  geom_smooth(method=lm, se=FALSE, aes(linetype = Tratamiento_pixel), show.legend = FALSE)+
  scale_color_manual(values = colores_tratamiento,
                     name = "Sitio y tratamiento",
                     labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD")) +  # Aplica los colores definidos
  scale_shape_manual(values = c(16, 2, 16, 2),
                     name = "Sitio y tratamiento",
                     labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD"))+
  scale_x_continuous(breaks = seq(-0.4, 0.4, by = 0.1), limits = c(-0.4, 0.405), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8, by = 1), expand = c(0,0)) +
  guides(linetype = guide_legend(override.aes = list(size = 7))) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})), x = 'LSWI')+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15))

gr7

#NDWI

gr8 <- ggplot(BD, aes(y=Flujo_CO2, x=Promedio_NDWI, color = Tipo_pixel, shape = Tipo_pixel)) + 
  geom_point(show.legend = FALSE, alpha = 0.6) +
  geom_smooth(method=lm, se=FALSE, aes(linetype = Tratamiento))+
  scale_color_manual(values = colores_tratamiento,
                     name = "Sitio y tratamiento",
                     labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD")) +  # Aplica los colores definidos
  scale_shape_manual(values = c(16, 2, 16, 2),
                     name = "Sitio y tratamiento",
                     labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD"))+
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8, by = 1), expand = c(0,0)) +
  scale_x_continuous(limits = c(-0.7005,0), breaks = seq(0,-1, by = -0.1), expand = c(0,0)) +
  guides(linetype = guide_legend(override.aes = list(size = 7))) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})), x = 'NDWI')+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15))

gr8

#install.packages('patchwork')
library(patchwork)

(gr5 + gr6) / (gr7 + gr8)


###################################################################
###################################################################



####R2 y pvalues####

##POR NUMERO DE COLLAR##

#CANARBC

#NDVI
BD_CANARBC <- BD[BD$sitiotrat == 'canar Bajo biomasa',]
cor_coefsNDVI <- cor.test(BD_CANARBC$Promedio_NDVI, BD_CANARBC$Flujo_CO2)
cor_coefsNDVI

#EVI
BD_CANARBC <- BD[BD$sitiotrat == 'canar Bajo biomasa',]
cor_coefsEVI <- cor.test(BD_CANARBC$Promedio_EVI, BD_CANARBC$Flujo_CO2)
cor_coefsEVI

#LSWI
BD_CANARBC <- BD[BD$sitiotrat == 'canar Bajo biomasa',]
cor_coefsLSWI <- cor.test(BD_CANARBC$Promedio_LSWI, BD_CANARBC$Flujo_CO2)
cor_coefsLSWI

#NDWI
BD_CANARBC <- BD[BD$sitiotrat == 'canar Bajo biomasa',]
cor_coefsNDWI <- cor.test(BD_CANARBC$Promedio_NDWI, BD_CANARBC$Flujo_CO2)
cor_coefsNDWI




#CANARSD

#NDVI
BD_CANARSD <- BD[BD$sitiotrat == 'canar Suelo desnudo',]
cor_coefsNDVI <- cor.test(BD_CANARSD$Promedio_NDVI, BD_CANARSD$Flujo_CO2)
cor_coefsNDVI

#EVI
BD_CANARSD <- BD[BD$sitiotrat == 'canar Suelo desnudo',]
cor_coefsEVI <- cor.test(BD_CANARSD$Promedio_EVI, BD_CANARSD$Flujo_CO2)
cor_coefsEVI

#LSWI
BD_CANARSD <- BD[BD$sitiotrat == 'canar Suelo desnudo',]
cor_coefsLSWI <- cor.test(BD_CANARSD$Promedio_LSWI, BD_CANARSD$Flujo_CO2)
cor_coefsLSWI

#NDWI
BD_CANARSD <- BD[BD$sitiotrat == 'canar Suelo desnudo',]
cor_coefsNDWI <- cor.test(BD_CANARSD$Promedio_NDWI, BD_CANARSD$Flujo_CO2)
cor_coefsNDWI




#FINANABC

#NDVI
BD_FINANABC <- BD[BD$sitiotrat == 'finana Bajo biomasa',]
cor_coefsNDVI <- cor.test(BD_FINANABC$Promedio_NDVI, BD_FINANABC$Flujo_CO2)
cor_coefsNDVI

#EVI
BD_FINANABC <- BD[BD$sitiotrat == 'finana Bajo biomasa',]
cor_coefsEVI <- cor.test(BD_FINANABC$Promedio_EVI, BD_FINANABC$Flujo_CO2)
cor_coefsEVI

#LSWI
BD_FINANABC <- BD[BD$sitiotrat == 'finana Bajo biomasa',]
cor_coefsLSWI <- cor.test(BD_FINANABC$Promedio_LSWI, BD_FINANABC$Flujo_CO2)
cor_coefsLSWI

#NDWI
BD_FINANABC <- BD[BD$sitiotrat == 'finana Bajo biomasa',]
cor_coefsNDWI <- cor.test(BD_FINANABC$Promedio_NDWI, BD_FINANABC$Flujo_CO2)
cor_coefsNDWI




#FINANASD

#NDVI
BD_FINANASD <- BD[BD$sitiotrat == 'finana Suelo desnudo',]
cor_coefsNDVI <- cor.test(BD_FINANASD$Promedio_NDVI, BD_FINANASD$Flujo_CO2)
cor_coefsNDVI

#EVI
BD_FINANASD <- BD[BD$sitiotrat == 'finana Suelo desnudo',]
cor_coefsEVI <- cor.test(BD_FINANASD$Promedio_EVI, BD_FINANASD$Flujo_CO2)
cor_coefsEVI

#LSWI
BD_FINANASD <- BD[BD$sitiotrat == 'finana Suelo desnudo',]
cor_coefsLSWI <- cor.test(BD_FINANASD$Promedio_LSWI, BD_FINANASD$Flujo_CO2)
cor_coefsLSWI

#NDWI
BD_FINANASD <- BD[BD$sitiotrat == 'finana Suelo desnudo',]
cor_coefsNDWI <- cor.test(BD_FINANASD$Promedio_NDWI, BD_FINANASD$Flujo_CO2)
cor_coefsNDWI



############################################################################




##POR NUMERO DE PIXEL##

#CANARBC

#NDVI
BD_CANARBC <- BD[BD$Tipo_pixel == 'Canar_Bajo_copa',]
cor_coefsNDVI <- cor.test(BD_CANARBC$Promedio_NDVI, BD_CANARBC$Flujo_CO2)
cor_coefsNDVI

#EVI
BD_CANARBC <- BD[BD$Tipo_pixel == 'Canar_Bajo_copa',]
cor_coefsEVI <- cor.test(BD_CANARBC$Promedio_EVI, BD_CANARBC$Flujo_CO2)
cor_coefsEVI

#LSWI
BD_CANARBC <- BD[BD$Tipo_pixel == 'Canar_Bajo_copa',]
cor_coefsLSWI <- cor.test(BD_CANARBC$Promedio_LSWI, BD_CANARBC$Flujo_CO2)
cor_coefsLSWI

#NDWI
BD_CANARBC <- BD[BD$Tipo_pixel == 'Canar_Bajo_copa',]
cor_coefsNDWI <- cor.test(BD_CANARBC$Promedio_NDWI, BD_CANARBC$Flujo_CO2)
cor_coefsNDWI




#CANARSD

#NDVI
BD_CANARSD <- BD[BD$Tipo_pixel == 'Canar_Suelo_desnudo',]
cor_coefsNDVI <- cor.test(BD_CANARSD$Promedio_NDVI, BD_CANARSD$Flujo_CO2)
cor_coefsNDVI

#EVI
BD_CANARSD <- BD[BD$Tipo_pixel == 'Canar_Suelo_desnudo',]
cor_coefsEVI <- cor.test(BD_CANARSD$Promedio_EVI, BD_CANARSD$Flujo_CO2)
cor_coefsEVI

#LSWI
BD_CANARSD <- BD[BD$Tipo_pixel == 'Canar_Suelo_desnudo',]
cor_coefsLSWI <- cor.test(BD_CANARSD$Promedio_LSWI, BD_CANARSD$Flujo_CO2)
cor_coefsLSWI

#NDWI
BD_CANARSD <- BD[BD$Tipo_pixel == 'Canar_Suelo_desnudo',]
cor_coefsNDWI <- cor.test(BD_CANARSD$Promedio_NDWI, BD_CANARSD$Flujo_CO2)
cor_coefsNDWI




#FINANABC

#NDVI
BD_FINANABC <- BD[BD$Tipo_pixel == 'Finana_Bajo_copa',]
cor_coefsNDVI <- cor.test(BD_FINANABC$Promedio_NDVI, BD_FINANABC$Flujo_CO2)
cor_coefsNDVI

#EVI
BD_FINANABC <- BD[BD$Tipo_pixel == 'Finana_Bajo_copa',]
cor_coefsEVI <- cor.test(BD_FINANABC$Promedio_EVI, BD_FINANABC$Flujo_CO2)
cor_coefsEVI

#LSWI
BD_FINANABC <- BD[BD$Tipo_pixel == 'Finana_Bajo_copa',]
cor_coefsLSWI <- cor.test(BD_FINANABC$Promedio_LSWI, BD_FINANABC$Flujo_CO2)
cor_coefsLSWI

#NDWI
BD_FINANABC <- BD[BD$Tipo_pixel == 'Finana_Bajo_copa',]
cor_coefsNDWI <- cor.test(BD_FINANABC$Promedio_NDWI, BD_FINANABC$Flujo_CO2)
cor_coefsNDWI




#FINANASD

#NDVI
BD_FINANASD <- BD[BD$Tipo_pixel == 'Finana_Suelo_desnudo',]
cor_coefsNDVI <- cor.test(BD_FINANASD$Promedio_NDVI, BD_FINANASD$Flujo_CO2)
cor_coefsNDVI

#EVI
BD_FINANASD <- BD[BD$Tipo_pixel == 'Finana_Suelo_desnudo',]
cor_coefsEVI <- cor.test(BD_FINANASD$Promedio_EVI, BD_FINANASD$Flujo_CO2)
cor_coefsEVI

#LSWI
BD_FINANASD <- BD[BD$Tipo_pixel == 'Finana_Suelo_desnudo',]
cor_coefsLSWI <- cor.test(BD_FINANASD$Promedio_LSWI, BD_FINANASD$Flujo_CO2)
cor_coefsLSWI

#NDWI
BD_FINANASD <- BD[BD$Tipo_pixel == 'Finana_Suelo_desnudo',]
cor_coefsNDWI <- cor.test(BD_FINANASD$Promedio_NDWI, BD_FINANASD$Flujo_CO2)
cor_coefsNDWI
