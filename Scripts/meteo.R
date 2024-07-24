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
library('openxlsx')


BD<-read_excel('bd_cadiar_meteo.xlsx')

# Calcular las medias y desviaciones estándar para cada combinación de "Semana", "Tratamiento" y "sitio"
medias_desviaciones <- BD %>%
    group_by(MES_CUENTA,) %>%
    summarise(Mean_TMax = mean(TMax, na.rm = TRUE),
            Mean_TMin = mean(TMin, na.rm = TRUE),
            Mean_Tmed = mean(Tmed, na.rm = TRUE),
            sum_prec = sum(Precip, na.rm = TRUE)) 



# Guardar el dataframe con media y desviación en un archivo Excel
write.xlsx(medias_desviaciones, "medias_BD_cadiar_meteo.xlsx", rowNames = FALSE)


BD<-read_excel('medias_BD_cadiar_meteo.xlsx')


gg <- ggplot(data = BD, mapping = aes(x = Mes, y = Mean_Tmed, group = 1)) + 
  geom_bar(stat = "identity", color="red", fill="red", width = 0.5) + 
  geom_line(mapping = aes(y = sum_prec/5), color="blue", size=1.5) + # Scale data to match desired scale
  scale_y_continuous("Temperature (ºC)", 
                     sec.axis = sec_axis(~ . *6, name = "Precipitacion (mm)") 
  )

gg



  theme_classic()+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17))
















#CHATGPT


# Crear el climograma con dos ejes Y
climograma <- ggplot(BD, aes(x = Mes)) +
  geom_bar(aes(y = Mean_Tmed), stat = "identity", fill = "red", width = 0.4) +
  geom_line(aes(y = sum_prec), color = "blue", size = 1, group = 1) +
  scale_y_continuous(
    name = "Temperatura (°C)",
    sec.axis = sec_axis(~.*8, name = "Precipitación (mm)")
  ) +
  labs(
    x = "Mes"
  ) +
  theme_classic()+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 17))
  

# Mostrar el climograma
print(climograma)



