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


####GRAFICOS DE LÍNEAS Y PUNTOS####


####NVDI####

BD<-read_excel('BD_satelite/bd_NDVI_sin_nubes.xlsx')

# Calcular las medias y desviaciones estándar para cada combinación de "Semana", "Tratamiento y tratamiento"
BD_medias_desviaciones <- BD %>%
  group_by(Fecha, Tipo_pixel) %>%
  summarise(Media_NVDI = mean(Value, na.rm = TRUE),
            SD_NVDI = sd(Value, na.rm = TRUE))


# Convertir la columna Fecha a formato Date si aún no lo está
BD_medias_desviaciones$Fecha <- as.Date(BD_medias_desviaciones$Fecha, format = "%d %b %Y")

# Obtener las fechas únicas de tus datos
fechas_unicas <- unique(BD_medias_desviaciones$Fecha)

#Hacer el grafico

colores_tratamiento <- c("goldenrod3", "goldenrod1", "royalblue3", "royalblue1")  # Define los colores

gr1 <- ggplot(BD_medias_desviaciones, aes(x = Fecha, y = Media_NVDI, colour = Tipo_pixel, linetype = Tipo_pixel)) +
  geom_errorbar(aes(ymin = Media_NVDI - SD_NVDI, ymax = Media_NVDI + SD_NVDI), width=0.1, size=0.5) +
  geom_line(size=0.8) +
  geom_point(size=2.3, show.legend = FALSE)+
  guides(x = guide_axis(angle = 45), linetype = guide_legend(override.aes = list(size =10)))+
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 0.8), expand = c(0,0)) +  # Personalización de la escala en el eje y
  scale_x_date(labels = scales::date_format("%b"), 
               breaks = "1 month") +  # Usa las fechas únicas como puntos de quiebre
  scale_color_manual(values = colores_tratamiento,
                     name = "Sitio y tratamiento",
                     labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD")
  ) +  # Aplica los colores definidos
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed"),
                        name = "Sitio y tratamiento",
                        labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD")
  ) +  # Controla las líneas continuas y discontinuas
  labs(y='NDVI', title ='a)')+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 17, face = "bold.italic", hjust = 0.5))

gr1


#gr8+geom_smooth()#modelo libre
#gr8+geom_smooth(method = "lm", formula = y ~ x, se = TRUE)

BD<-read_excel('BD_satelite/bd_NVDI_sin_nubes.xlsx')

# Calcular las medias y desviaciones estándar para cada combinación de "Semana", "Tratamiento y tratamiento"
BD_medias_desviaciones <- BD %>%
  group_by(Fecha, Tipo_pixel) %>%
  summarise(Media_NVDI = mean(Value, na.rm = TRUE),
            SD_NVDI = sd(Value, na.rm = TRUE))




####EVI####

BD<-read_excel('BD_satelite/bd_EVI_sin_nubes.xlsx')

# Calcular las medias y desviaciones estándar para cada combinación de "Semana", "Tratamiento y tratamiento"
BD_medias_desviaciones <- BD %>%
  group_by(Fecha, Tipo_pixel) %>%
  summarise(Media_EVI = mean(Value, na.rm = TRUE),
            SD_EVI = sd(Value, na.rm = TRUE))


# Convertir la columna Fecha a formato Date si aún no lo está
BD_medias_desviaciones$Fecha <- as.Date(BD_medias_desviaciones$Fecha, format = "%d %b %Y")

# Obtener las fechas únicas de tus datos
fechas_unicas <- unique(BD_medias_desviaciones$Fecha)

#Hacer el grafico

colores_tratamiento <- c("goldenrod3", "goldenrod1", "royalblue3", "royalblue1")  # Define los colores

gr2 <- ggplot(BD_medias_desviaciones, aes(x = Fecha, y = Media_EVI, colour = Tipo_pixel, linetype = Tipo_pixel)) +
  geom_errorbar(aes(ymin = Media_EVI - SD_EVI, ymax = Media_EVI + SD_EVI), width=0.1, size=0.5) +
  geom_line(size=0.8) +
  geom_point(size=2.3, show.legend = FALSE)+
  guides(x = guide_axis(angle = 45), linetype = guide_legend(override.aes = list(size =10)))+
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 0.8), expand = c(0,0)) +  # Personalización de la escala en el eje y
  scale_x_date(labels = scales::date_format("%b"), 
               breaks = "1 month") +  # Usa las fechas únicas como puntos de quiebre
  scale_color_manual(values = colores_tratamiento,
                     name = "Sitio y tratamiento",
                     labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD")
  ) +  # Aplica los colores definidos
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed"),
                        name = "Sitio y tratamiento",
                        labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD")
  ) +  # Controla las líneas continuas y discontinuas
  labs(y='EVI', title ='b)')+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 17, face = "bold.italic", hjust = 0.5))

gr2


#gr8+geom_smooth()#modelo libre
#gr8+geom_smooth(method = "lm", formula = y ~ x, se = TRUE)




####LSWI####

BD<-read_excel('BD_satelite/bd_LSWI_sin_nubes.xlsx')

# Calcular las medias y desviaciones estándar para cada combinación de "Semana", "Tratamiento y tratamiento"
BD_medias_desviaciones <- BD %>%
  group_by(Fecha, Tipo_pixel) %>%
  summarise(Media_LSWI = mean(Value, na.rm = TRUE),
            SD_LSWI = sd(Value, na.rm = TRUE))


# Convertir la columna Fecha a formato Date si aún no lo está
BD_medias_desviaciones$Fecha <- as.Date(BD_medias_desviaciones$Fecha, format = "%d %b %Y")

# Obtener las fechas únicas de tus datos
fechas_unicas <- unique(BD_medias_desviaciones$Fecha)

#Hacer el grafico

colores_tratamiento <- c("goldenrod3", "goldenrod1", "royalblue3", "royalblue1")  # Define los colores

gr3 <- ggplot(BD_medias_desviaciones, aes(x = Fecha, y = Media_LSWI, colour = Tipo_pixel, linetype = Tipo_pixel)) +
  geom_errorbar(aes(ymin = Media_LSWI - SD_LSWI, ymax = Media_LSWI + SD_LSWI), width=0.1, size=0.5) +
  geom_line(size=0.8) +
  geom_point(size=2.3, show.legend = FALSE)+
  guides(x = guide_axis(angle = 45), linetype = guide_legend(override.aes = list(size =10)))+
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.1), limits = c(-0.4, 0.4), expand = c(-0.4,0.4)) +  # Personalización de la escala en el eje y
  scale_x_date(labels = scales::date_format("%b"), 
               breaks = "1 month") +  # Usa las fechas únicas como puntos de quiebre
  scale_color_manual(values = colores_tratamiento,
                     name = "Sitio y tratamiento",
                     labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD")
  ) +  # Aplica los colores definidos
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed"),
                        name = "Sitio y tratamiento",
                        labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD")
  ) +  # Controla las líneas continuas y discontinuas
  labs(y='LSWI', title ='c)')+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 17, face = "bold.italic", hjust = 0.5))

gr3


#gr8+geom_smooth()#modelo libre
#gr8+geom_smooth(method = "lm", formula = y ~ x, se = TRUE)







####NWDI####

BD<-read_excel('BD_satelite/bd_NDWI_sin_nubes.xlsx')

# Calcular las medias y desviaciones estándar para cada combinación de "Semana", "Tratamiento y tratamiento"
BD_medias_desviaciones <- BD %>%
  group_by(Fecha, Tipo_pixel) %>%
  summarise(Media_NWDI = mean(Value, na.rm = TRUE),
            SD_NWDI = sd(Value, na.rm = TRUE))


# Convertir la columna Fecha a formato Date si aún no lo está
BD_medias_desviaciones$Fecha <- as.Date(BD_medias_desviaciones$Fecha, format = "%d %b %Y")

# Obtener las fechas únicas de tus datos
fechas_unicas <- unique(BD_medias_desviaciones$Fecha)

#Hacer el grafico

colores_tratamiento <- c("goldenrod3", "goldenrod1", "royalblue3", "royalblue1")  # Define los colores

gr4 <- ggplot(BD_medias_desviaciones, aes(x = Fecha, y = Media_NWDI, colour = Tipo_pixel, linetype = Tipo_pixel)) +
  geom_errorbar(aes(ymin = Media_NWDI - SD_NWDI, ymax = Media_NWDI + SD_NWDI), width=0.1, size=0.5) +
  geom_line(size=0.8, show.legend = FALSE) +
  geom_point(size=2.3, show.legend = FALSE)+
  guides(x = guide_axis(angle = 45), linetype = guide_legend(override.aes = list(size =10)))+
  scale_y_continuous(trans = "reverse", breaks = seq(0, -1, by = -0.1), limits = c(0, -0.8), expand = c(0,-0.8)) +  # Personalización de la escala en el eje y
  scale_x_date(labels = scales::date_format("%b"), 
               breaks = "1 month") +  # Usa las fechas únicas como puntos de quiebre
  scale_color_manual(values = colores_tratamiento,
                     name = "Sitio y tratamiento",
                     labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD")
  ) +  # Aplica los colores definidos
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed"),
                        name = "Sitio y tratamiento",
                        labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD")
  ) +  # Controla las líneas continuas y discontinuas
  labs(y='NDWI', title ='d)')+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 17, face = "bold.italic", hjust = 0.5))

gr4


#gr8+geom_smooth()#modelo libre
#gr8+geom_smooth(method = "lm", formula = y ~ x, se = TRUE)


library('patchwork')

gr1/gr2

gr3/gr4




####Albedo####

BD<-read_excel('BD_satelite/bd_albedo.xlsx')

# Calcular las medias y desviaciones estándar para cada combinación de "Semana", "Tratamiento y tratamiento"
BD_medias_desviaciones <- BD %>%
  group_by(Fecha, Tipo_pixel) %>%
  summarise(Media_NWDI = mean(Value, na.rm = TRUE),
            SD_NWDI = sd(Value, na.rm = TRUE))


# Convertir la columna Fecha a formato Date si aún no lo está
BD_medias_desviaciones$Fecha <- as.Date(BD_medias_desviaciones$Fecha, format = "%d %b %Y")

# Obtener las fechas únicas de tus datos
fechas_unicas <- unique(BD_medias_desviaciones$Fecha)

#Hacer el grafico

colores_tratamiento <- c("goldenrod3", "goldenrod1", "royalblue3", "royalblue1")  # Define los colores

gr4 <- ggplot(BD_medias_desviaciones, aes(x = Fecha, y = Media_NWDI, colour = Tipo_pixel, linetype = Tipo_pixel)) +
  geom_errorbar(aes(ymin = Media_NWDI - SD_NWDI, ymax = Media_NWDI + SD_NWDI), width=0.1, size=0.5) +
  geom_line(size=0.8, show.legend = FALSE) +
  geom_point(size=2.3, show.legend = FALSE)+
  guides(x = guide_axis(angle = 45), linetype = guide_legend(override.aes = list(size =10)))+
  scale_y_continuous( breaks = seq(0, 0.3, by = 0.05), limits = c(0, 0.25), expand = c(0,0)) +  # Personalización de la escala en el eje y
  scale_x_date(labels = scales::date_format("%b"), 
               breaks = "1 month") +  # Usa las fechas únicas como puntos de quiebre
  scale_color_manual(values = colores_tratamiento,
                     name = "Sitio y tratamiento",
                     labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD")
  ) +  # Aplica los colores definidos
  scale_linetype_manual(values = c("solid", "dashed", "solid", "dashed"),
                        name = "Sitio y tratamiento",
                        labels = c("Robledal BC", "Robledal SD", "Encinar BC", "Encinar SD")
  ) +  # Controla las líneas continuas y discontinuas
  labs(y='Albedo', title ='e)')+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 17, face = "bold.italic", hjust = 0.5))

gr4

gr4/gr1
