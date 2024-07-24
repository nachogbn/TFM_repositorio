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

BD<-read_excel('BD_meteo/bd_cadiar_meteo.xlsx')

BD_medias_desviaciones <- BD %>%
  group_by(MES_CUENTA) %>%
  summarise(Media_Tmax = mean(TMax, na.rm = TRUE),
            Media_Tmin = mean(TMin, na.rm = TRUE))
            
            
####GRAFICOS DE LÍNEAS Y PUNTOS####

# Convertir la columna Mes a formato Date si aún no lo está
BD$Mes <- as.Date(BD$FECHA_original, format = "%d %b %Y")

# Obtener las Mess únicas de tus datos
Mess_unicas <- unique(BD$Mes)


gr9<- ggplot(data = BD, mapping = aes(x = Mes, y = Precip, group = 1)) + 
  geom_bar(stat = "identity", color="blue4", fill="blue4", width = 0.2) + 
  geom_line(mapping = aes(y = (Tmed+5) *1.5), color="orangered2", size=1) + # Scale data to match desired scale
  #geom_point(mapping = aes(y = Tmed+10), color="red", size=1.5)+
  scale_y_continuous(breaks = seq(0, 60, by = 10), limits = c(0,60), expand = c(0, 0), "Precipitación [mm]", 
                     sec.axis = sec_axis(~ . /1.5 -5, name = "Temperatura [°C]", breaks = seq(-5, 35, by = 5))) + # Reverse transformation to match data
  scale_x_date(labels = scales::date_format("%b"), breaks = "1 month") +
  guides(x = guide_axis(angle = 45))+
  theme_classic()+
  labs(title ='a)') +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        axis.title.y = element_text(vjust = +2),
        axis.title.y.right = element_text(vjust = +2),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 17, face = "bold.italic", hjust = 0.5))

gr9


#FIÑANA

BD<-read_excel('BD_meteo/bd_finana_meteo.xlsx')

####GRAFICOS DE LÍNEAS Y PUNTOS####

# Convertir la columna Mes a formato Date si aún no lo está
BD$Mes <- as.Date(BD$FECHA_original, format = "%d %b %Y")


gr10<- ggplot(data = BD, mapping = aes(x = Mes, y = Precip, group = 1)) + 
  geom_bar(stat = "identity", color="blue4", fill="blue4", width = 0.2) + 
  geom_line(mapping = aes(y = (Tmed+5) *1.5), color="orangered2", size=1) + # Scale data to match desired scale
  #geom_point(mapping = aes(y = Tmed+10), color="red", size=1.5)+
  scale_y_continuous(breaks = seq(0, 60, by = 10), limits = c(0,60), expand = c(0, 0), "Precipitación [mm]", 
                     sec.axis = sec_axis(~ . /1.5 -5, name = "Temperatura [°C]", breaks = seq(-5, 35, by = 5))) + # Reverse transformation to match data
  scale_x_date(labels = scales::date_format("%b"), breaks = "1 month") +
  guides(x = guide_axis(angle = 45))+
  theme_classic()+
  labs(title ='b)') +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        axis.title.y = element_text(vjust = +2),
        axis.title.y.right = element_text(vjust = +2),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 17, face = "bold.italic", hjust = 0.5))

gr10

library('patchwork')

gr9 / gr10


