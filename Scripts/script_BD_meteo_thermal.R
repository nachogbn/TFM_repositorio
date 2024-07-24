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


#1) transponer datos csv
library(tidyr)


BD <- read_excel('BD_satelite/bd_landsat.xlsx', col_names = FALSE)

library(data.table)

#transpose data frame
BD_t <- transpose(BD)

colnames(BD_t) <- BD_t[1,]
BD_t <- BD_t[-1, ] 



BDgathered <- BD_t %>%
  gather(Date, "2017-03-30 10:56:00Q", starts_with("20"), na.rm = T)

BDgathered$Date <- as.Date(BDgathered$Date)

#library('openxlsx')
#write.xlsx(BDgathered, "BD_thermal.xlsx", rowNames = FALSE)


#Leemos bases de datos meteo

BD_meteo_cadiar <- read_excel('BD_meteo/bd_cadiar_meteo.xlsx')

BD_meteo_finana <- read_excel('BD_meteo/bd_finana_meteo.xlsx')

#Las juntamos
BD_meteo <- rbind(BD_meteo_cadiar, BD_meteo_finana)

BD_thermal <- BDgathered %>% inner_join( BD_meteo, by=c('Location', 'Date'='FECHA_original'))


#Cambio el nombre a la columna 5 y la marco como numerica para la correlacion
colnames(BD_thermal)[5] ="Value"

BD_thermal$Value <- as.numeric(BD_thermal$Value)

#Transformamos de Kelvin a Celsius
BD_thermal$Celsius <- BD_thermal$Value-273.15

BD_canar <- subset(BD_thermal, Location == 'canar')
BD_finana <- subset(BD_thermal, Location == 'finana')

modelos_canar <- lm(Celsius~Tmed, data = BD_canar)
modelos_canar

#Grafico R pearson

cor_coefs_canar <- cor.test(BD_canar$Tmed, BD_canar$Celsius)
cor_coefs_finana <- cor.test(BD_finana$Tmed, BD_finana$Celsius)

colores_tratamiento <- c("goldenrod1", "royalblue1")  # Define los colores


gr1 <- ggplot(BD_thermal, aes(x=Tmed, y=Celsius, color=Location)) + 
  geom_point(alpha = 0.7) +
  geom_smooth(method=lm, se=FALSE)+
  scale_y_continuous(breaks = seq(0, 46, by = 5), limits = c(0, 46), expand = c(0,0)) + 
  scale_x_continuous(limits = c(0,46), breaks = seq(0,46, by = 5), expand = c(0,0)) +
  scale_color_manual(values = colores_tratamiento,
                     name = "Sitio y tratamiento",
                     labels = c("Robledal", "Encinar")) +
  guides(linetype = guide_legend(override.aes = list(size = 7))) +
  labs(x= 'Temperatura (ºC)', y = 'Landsat-9 Thermal (ºC)',  title ='b)')+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 17, face = "bold.italic", hjust = 0.5)) +
  annotate("text", size = 4.5, x = 37.2, y = 42, label = paste0("Robledal R: ", round(cor_coefs_canar$estimate, 3))) +
  annotate("text", size = 4.5, x = 38.3, y = 39, label = paste0("Robledal m: ", round(pendiente_canar, 3))) +
  annotate("text", size = 4.5, x = 37.8, y = 34, label = paste0("Encinar R: ", round(cor_coefs_finana$estimate, 3))) +
  annotate("text", size = 4.5, x = 37.9, y = 31, label = paste0("Encinar m: ", round(pendiente_finana, 3)))
gr1

# Ajustar modelos de regresión lineal
modelo_canar <- lm(Celsius ~ Tmed, data = subset(BD_thermal, Location == "canar"))
modelo_finana <- lm(Celsius ~ Tmed, data = subset(BD_thermal, Location == "finana"))

# Obtener las pendientes
pendiente_canar <- coef(summary(modelo_canar))[2, 1]
pendiente_finana <- coef(summary(modelo_finana))[2, 1]

# Imprimir las pendientes
print(paste("Pendiente Robledal:", round(pendiente_canar, 3)))
print(paste("Pendiente Encinar:", round(pendiente_finana, 3)))

# Mostrar el gráfico
print(gr1)

cor_coefs <- cor.test(BD_thermal$Tmed, BD_thermal$Celsius)

modelo <- lm(Celsius ~ Tmed, data = BD_thermal)
pendiente<- coef(summary(modelo))[2, 1]
pendiente

gr2 <- ggplot(BD_thermal, aes(x=Tmed, y=Celsius, color=Location)) + 
  geom_point(alpha = 0.7, show.legend = FALSE) +
  geom_smooth(method=lm, se=FALSE, color = 'black', show.legend = FALSE)+
  scale_y_continuous(breaks = seq(0, 46, by = 5), limits = c(0, 46), expand = c(0,0)) + 
  scale_x_continuous(limits = c(0,46), breaks = seq(0,46, by = 5), expand = c(0,0)) +
  scale_color_manual(values = colores_tratamiento,
                     name = "Sitio y tratamiento",
                     labels = c("Robledal", "Encinar")) +
  guides(linetype = guide_legend(override.aes = list(size = 7))) +
  labs(x= 'Temperatura (ºC)', y = 'Landsat-9 Thermal (ºC)', title ='a)')+
  theme_classic()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 17),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 17, face = "bold.italic", hjust = 0.5)) +
  annotate("text", size = 4.5, x = 37.9, y = 42, label = paste0("R: ", round(cor_coefs$estimate, 3))) +
  annotate("text", size = 4.5, x = 38.35, y = 39, label = paste0("p-value: ", round(cor_coefs$p.value, 7)))+
  annotate("text", size = 4.5, x = 37.9, y = 36, label = paste0("m: ", round(pendiente, 3)))


gr2

library('patchwork')

(gr2 + gr1)/(gr2 + gr1)

#Grafico R pearson
cor_coefs <- cor.test(BD_thermal$TMax, BD_thermal$Celsius)
cor_coefs

gr2 <- ggplot(BD_thermal, aes(x=TMax, y=Celsius)) + 
  geom_point(alpha = 0.7) +
  geom_smooth(method=lm, se=FALSE)+
  scale_y_continuous(breaks = seq(0, 46, by = 5), limits = c(0, 46), expand = c(0,0)) + 
  scale_x_continuous(limits = c(-5,46), breaks = seq(-5,46, by = 5), expand = c(0,0)) +
  guides(linetype = guide_legend(override.aes = list(size = 7))) +
  labs(x= 'Temperatura máxima (ºC)', y = '')+
  theme_light()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        axis.title.x = element_text(vjust = -0.5)) +
  annotate("text", size = 4.5, x = 40, y = 42, label = paste0("R: ", round(cor_coefs$estimate, 3))) +
  annotate("text", size = 4.5, x = 40.4, y = 40, label = paste0("p-value: ", round(cor_coefs$p.value, 7)))

gr2

#Grafico R pearson
cor_coefs <- cor.test(BD_thermal$TMin, BD_thermal$Celsius)


gr3 <- ggplot(BD_thermal, aes(x=TMin, y=Celsius)) + 
  geom_point(alpha = 0.7) +
  geom_smooth(method=lm, se=FALSE)+
  scale_y_continuous(breaks = seq(-5, 46, by = 5), limits = c(0, 46), expand = c(0,0)) + 
  scale_x_continuous(limits = c(-5,46), breaks = seq(-5,46, by = 5), expand = c(0,0)) +
  guides(linetype = guide_legend(override.aes = list(size = 7))) +
  labs(x= 'Temperatura mínima (ºC)', y = 'Thermal (ºC)')+
  theme_light()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        axis.title.y = element_text(vjust = +2),
        axis.title.x = element_text(vjust = -0.5)) +
  annotate("text", size = 4.5, x = 40, y = 42, label = paste0("R: ", round(cor_coefs$estimate, 3))) +
  annotate("text", size = 4.5, x = 40.4, y = 40, label = paste0("p-value: ", round(cor_coefs$p.value, 7)))

gr3



library('patchwork')

(gr3 + gr2)

