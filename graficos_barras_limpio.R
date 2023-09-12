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

####REUNIÓN JUNIO####

BD<-read_excel('media_desviacion_R90.xlsx')

BD_portugos <- subset(BD, Sitio == 'Pine forest (Portugos)')
BD_canar <- subset(BD, Sitio == 'Oak forest')
BD_finana <- subset(BD, Sitio == 'Oak forest (Finana)')
BD_abrucena <- subset(BD, Sitio == 'Pine forest (Abrucena)')


##BARRAS
colores_tratamiento <- c("palegreen3", "tan3")  

# Convertir la columna de Fecha en un factor con orden cronológico
BD_portugos$Fecha <- factor(BD_portugos$Fecha, levels = unique(BD_portugos$Fecha))

ggplot(BD_portugos, aes(x = Semana, y = Media_Flujo_CO2, fill = Tratamiento)) +
  geom_col(position = position_dodge(width = 2), width = 1.8) +
  geom_errorbar(aes(ymin = Media_Flujo_CO2 - SD_Flujo_CO2, ymax = Media_Flujo_CO2 + SD_Flujo_CO2),
                position = position_dodge(width = 2), width = 0.8) +
  ylim(0, 8) +
  facet_grid(cols = vars(Fecha), scales = "free_x", switch = "x") +
  scale_y_continuous(breaks = seq(0, 8, by = 0.5), limits = c(0, 8)) +  # Personalización de la escala en el eje y
  scale_x_discrete(labels = BD_portugos$Fecha) +
  theme_gray() +
  scale_fill_manual(values = colores_tratamiento) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1}))) +
  theme(axis.text = element_text(size = 20),
        strip.text.x = element_text(size = 13),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17))

#CAÑAR

BD_canar$Fecha <- factor(BD_canar$Fecha, levels = unique(BD_canar$Fecha))

ggplot(BD_canar, aes(x = Semana, y = Media_Flujo_CO2, fill = Tratamiento)) +
  geom_col(position = position_dodge(width = 2), width = 1.8) +
  geom_errorbar(aes(ymin = Media_Flujo_CO2 - SD_Flujo_CO2, ymax = Media_Flujo_CO2 + SD_Flujo_CO2),
                position = position_dodge(width = 2), width = 0.8) +
  ylim(0, 8) +
  facet_grid(cols = vars(Fecha), scales = "free_x", switch = "x") +
  scale_y_continuous(breaks = seq(0, 8, by = 0.5), limits = c(0, 8)) +  # Personalización de la escala en el eje y
  scale_x_discrete(labels = BD_canar$Fecha) +
  theme_gray() +
  scale_fill_manual(values = colores_tratamiento) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1}))) +
  theme(axis.text = element_text(size = 20),
        strip.text.x = element_text(size = 13),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17))



#ABRUCENA

BD_abrucena$Fecha <- factor(BD_abrucena$Fecha, levels = unique(BD_abrucena$Fecha))

ggplot(BD_abrucena, aes(x = Semana, y = Media_Flujo_CO2, fill = Tratamiento)) +
  geom_col(position = position_dodge(width = 2), width = 1.8) +
  geom_errorbar(aes(ymin = Media_Flujo_CO2 - SD_Flujo_CO2, ymax = Media_Flujo_CO2 + SD_Flujo_CO2),
                position = position_dodge(width = 2), width = 0.8) +
  ylim(0, 8) +
  facet_grid(cols = vars(Fecha), scales = "free_x", switch = "x") +
  scale_y_continuous(breaks = seq(0, 8, by = 0.5), limits = c(0, 8)) +  # Personalización de la escala en el eje y
  scale_x_discrete(labels = BD_abrucena$Fecha) +
  theme_gray() +
  scale_fill_manual(values = colores_tratamiento) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1}))) +
  theme(axis.text = element_text(size = 20),
        strip.text.x = element_text(size = 13),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17))



#FIÑANA

BD_finana$Fecha <- factor(BD_finana$Fecha, levels = unique(BD_finana$Fecha))

ggplot(BD_finana, aes(x = Semana, y = Media_Flujo_CO2, fill = Tratamiento)) +
  geom_col(position = position_dodge(width = 2), width = 1.8) +
  geom_errorbar(aes(ymin = Media_Flujo_CO2 - SD_Flujo_CO2, ymax = Media_Flujo_CO2 + SD_Flujo_CO2),
                position = position_dodge(width = 2), width = 0.8) +
  ylim(0, 8) +
  facet_grid(cols = vars(Fecha), scales = "free_x", switch = "x") +
  scale_y_continuous(breaks = seq(0, 8, by = 0.5), limits = c(0, 8)) +  # Personalización de la escala en el eje y
  scale_x_discrete(labels = BD_finana$Fecha) +
  theme_gray() +
  scale_fill_manual(values = colores_tratamiento) +
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1}))) +
  theme(axis.text = element_text(size = 20),
        strip.text.x = element_text(size = 13),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17))




####GRAFICOS DE REGRESIÓN LINEAL####

BD<-read_excel('media_desviacion_R90_fecha.xlsx')

BD_portugos <- subset(BD, Sitio == 'Pine forest (Portugos)')
BD_canar <- subset(BD, Sitio == 'Oak forest')
BD_finana <- subset(BD, Sitio == 'Oak forest (Finana)')
BD_abrucena <- subset(BD, Sitio == 'Pine forest (Abrucena)')

#Portugos

# Convierte la columna Fecha a formato Date si aún no lo está
BD_portugos$Fecha <- as.Date(BD_portugos$Fecha, format = "%d %b %Y")

# Obtén las fechas únicas de tus datos
fechas_unicas <- unique(BD_portugos$Fecha)


gr8<-ggplot(BD_portugos, aes(x=Fecha, y=Media_Flujo_CO2, colour=Tratamiento)) + 
  geom_errorbar(aes(ymin = Media_Flujo_CO2 - SD_Flujo_CO2, ymax = Media_Flujo_CO2 + SD_Flujo_CO2), width=.1) +
  geom_line() +
  geom_point()+
  guides(x = guide_axis(angle = 45))+
  scale_y_continuous(breaks = seq(0, 8, by = 0.5), limits = c(0, 8)) +  # Personalización de la escala en el eje y
  scale_x_date(labels = scales::date_format("%d %b"), 
               breaks = fechas_unicas,  # Usa las fechas únicas como puntos de quiebre
               date_minor_breaks = "1 day") +  # Agrega marcas menores para los días  theme_gray()+
  scale_color_manual(values = colores_tratamiento) +  # Aplica los colores definidos
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})))+
  theme_classic()+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17))
gr8
gr8+geom_smooth()#modelo libre
gr8+geom_smooth(method = "lm", formula = y ~ x, se = TRUE)

###Cánar

# Convierte la columna Fecha a formato Date si aún no lo está
BD_canar$Fecha <- as.Date(BD_canar$Fecha, format = "%d %b %Y")

# Obtén las fechas únicas de tus datos
fechas_unicas <- unique(BD_canar$Fecha)


gr8<-ggplot(BD_canar, aes(x=Fecha, y=Media_Flujo_CO2, colour=Tratamiento)) + 
  geom_errorbar(aes(ymin = Media_Flujo_CO2 - SD_Flujo_CO2, ymax = Media_Flujo_CO2 + SD_Flujo_CO2), width=.1) +
  geom_line() +
  geom_point()+
  guides(x = guide_axis(angle = 45))+
  scale_y_continuous(breaks = seq(0, 8, by = 0.5), limits = c(0, 8)) +  # Personalización de la escala en el eje y
  scale_x_date(labels = scales::date_format("%d %b"), 
               breaks = fechas_unicas,  # Usa las fechas únicas como puntos de quiebre
               date_minor_breaks = "1 day") +  # Agrega marcas menores para los días  theme_gray()+
  scale_color_manual(values = colores_tratamiento) +  # Aplica los colores definidos
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})))+
  theme_classic()+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17))
gr8
gr8+geom_smooth()#modelo libre
gr8+geom_smooth(method = "lm", formula = y ~ x, se = TRUE)

###Abrucena

# Convierte la columna Fecha a formato Date si aún no lo está
BD_abrucena$Fecha <- as.Date(BD_abrucena$Fecha, format = "%d %b %Y")

# Obtén las fechas únicas de tus datos
fechas_unicas <- unique(BD_abrucena$Fecha)


gr8<-ggplot(BD_abrucena, aes(x=Fecha, y=Media_Flujo_CO2, colour=Tratamiento)) + 
  geom_errorbar(aes(ymin = Media_Flujo_CO2 - SD_Flujo_CO2, ymax = Media_Flujo_CO2 + SD_Flujo_CO2), width=.1) +
  geom_line() +
  geom_point()+
  guides(x = guide_axis(angle = 45))+
  scale_y_continuous(breaks = seq(0, 8, by = 0.5), limits = c(0, 8)) +  # Personalización de la escala en el eje y
  scale_x_date(labels = scales::date_format("%d %b"), 
               breaks = fechas_unicas,  # Usa las fechas únicas como puntos de quiebre
               date_minor_breaks = "1 day") +  # Agrega marcas menores para los días  theme_gray()+
  scale_color_manual(values = colores_tratamiento) +  # Aplica los colores definidos
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})))+
  theme_classic()+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17))
gr8
gr8+geom_smooth()#modelo libre
gr8+geom_smooth(method = "lm", formula = y ~ x, se = TRUE)



###Finana

# Convierte la columna Fecha a formato Date si aún no lo está
BD_finana$Fecha <- as.Date(BD_finana$Fecha, format = "%d %b %Y")

# Obtén las fechas únicas de tus datos
fechas_unicas <- unique(BD_finana$Fecha)


gr8<-ggplot(BD_finana, aes(x=Fecha, y=Media_Flujo_CO2, colour=Tratamiento)) + 
  geom_errorbar(aes(ymin = Media_Flujo_CO2 - SD_Flujo_CO2, ymax = Media_Flujo_CO2 + SD_Flujo_CO2), width=.1) +
  geom_line() +
  geom_point()+
  guides(x = guide_axis(angle = 45))+
  scale_y_continuous(breaks = seq(0, 8, by = 0.5), limits = c(0, 8)) +  # Personalización de la escala en el eje y
  scale_x_date(labels = scales::date_format("%d %b"), 
               breaks = fechas_unicas,  # Usa las fechas únicas como puntos de quiebre
               date_minor_breaks = "1 day") +  # Agrega marcas menores para los días  theme_gray()+
  scale_color_manual(values = colores_tratamiento) +  # Aplica los colores definidos
  labs(y=expression(F[CO[2]]~(mu~mol~m^{-2}~s^{-1})))+
  theme_classic()+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17))
gr8
gr8+geom_smooth()#modelo libre
gr8+geom_smooth(method = "lm", formula = y ~ x, se = TRUE)