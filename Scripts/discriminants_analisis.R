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


library(tidyverse)
library(caret)
library(klaR)

#Leemos base de datos
BD<-read_excel('BD_ESTADISTICA.xlsx')

#Primero Canar
BD_canar <- subset(BD, Location == 'canar')

training_sample <- sample(c(TRUE, FALSE), nrow(BD_canar), replace = T, prob = c(0.6,0.4))
train <- BD_canar[training_sample, ]
test <- BD_canar[!training_sample, ]

lda.BD <- lda(Tratamiento.x ~ Promedio_NDVI + Promedio_EVI + Promedio_LSWI + Promedio_NDWI, train)

lda.BD #show results


#Finana

BD_finana <- subset(BD, Location == 'finana')

training_sample <- sample(c(TRUE, FALSE), nrow(BD_finana), replace = T, prob = c(0.6,0.4))
train <- BD_finana[training_sample, ]
test <- BD_finana[!training_sample, ]

lda.BD <- lda(Tratamiento.x ~ Promedio_NDVI + Promedio_EVI + Promedio_LSWI + Promedio_NDWI, train)

lda.BD #show results







