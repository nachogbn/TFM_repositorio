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


#Leemos bases de datos meteo

BD_meteo_cadiar <- read_excel('BD_meteo/bd_cadiar_meteo.xlsx')

BD_meteo_finana <- read_excel('BD_meteo/bd_finana_meteo.xlsx')

#Las juntamos
BD_meteo <- rbind(BD_meteo_cadiar, BD_meteo_finana)


#Leemos base de datos quercus
BDQ<-read_excel('BD_definitiva_quercus.xlsx')


BD <- BDQ %>% inner_join(BD_meteo, by=c('Location', 'Fecha'='FECHA_original'))


#Grafico R pearson
cor_coefs <- cor.test(BD$Tmed, BD$Flujo_CO2)


gr1 <- ggplot(BD, aes(x=Tmed, y=Flujo_CO2)) + 
  geom_point(alpha = 0.7) +
  geom_smooth(method=lm, se=FALSE)+
  scale_x_continuous(breaks = seq(0, 30, by = 5), limits = c(0, 30.5), expand = c(0,0)) + 
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8, by = 1), expand = c(0,0)) +
  guides(linetype = guide_legend(override.aes = list(size = 7))) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})), x = 'Temperatura media (ºC)')+
  theme_light()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        axis.title.y = element_text(vjust = +2),
        axis.title.x = element_text(vjust = -0.5)) +
  annotate("text", size = 4.5, y = 7, x = 2.52, label = paste0("R: ", round(cor_coefs$estimate, 3))) +
  annotate("text", size = 4.5, y = 6.65, x = 3, label = paste0("p-value: ", round(cor_coefs$p.value, 7)))

gr1



#Gráfico por sitio y tratamiento

BD$sitiotrat <- paste(BD$Location, BD$Tratamiento)
BD


colores_tratamiento <- c("goldenrod3", "goldenrod1", "royalblue3", "royalblue1") # Define los colores


gr8 <- ggplot(BD, aes(y=Flujo_CO2, x=Tmed, color = sitiotrat, shape = sitiotrat)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method=lm, se=FALSE, aes(linetype = Tratamiento), show.legend = TRUE)+
  scale_color_manual(values = colores_tratamiento,
                     name = "Sitio y tratamiento",
                     labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD")) +  # Aplica los colores definidos
  scale_shape_manual(values = c(16, 2, 16, 2),
                     name = "Sitio y tratamiento",
                     labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD"))+
  scale_x_continuous(breaks = seq(0, 30, by = 5), limits = c(0, 30), expand = c(0,0)) + 
  scale_y_continuous(limits = c(0,8), breaks = seq(0,8, by = 1), expand = c(0,0)) +
  guides(linetype = guide_legend(override.aes = list(size = 7))) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})), x = "Temperatura (ºC)")+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15))

gr8/gr8

gr8


#Canar BC y Fiñana SD
BD_CANARBC <- BD[BD$sitiotrat == 'canar Bajo biomasa',]
cor_coefsNDVI <- cor.test(BD_CANARBC$Flujo_CO2, BD_CANARBC$Tmed)
cor_coefsNDVI

#Canar BC y Fiñana SD
BD_FINANABC <- BD[BD$sitiotrat == 'finana Suelo desnudo',]
cor_coefsNDVI <- cor.test(BD_FINANABC$Flujo_CO2, BD_FINANABC$Tmed)
cor_coefsNDVI


