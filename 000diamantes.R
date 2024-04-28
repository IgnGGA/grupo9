rm(list=ls()) #Limpiar consola
#setwd('C:/Users/Ignacio/OneDrive - UNIVERSIDAD ANDRES BELLO/Documents/003_tercerAño_2024/05_mineriaDeDatos/Grupo_9')
#INSTALACION-LIBRERIAS----------------------------------------------------------
#install.packages("ggplot2")
#install.packages("qqplotr")
#install.packages("corrplot")
#install.packages("outliers")
#install.packages("dplyr")
#install.packages("GGally")
#install.packages('factoextra')
#install.packages('mice')
#install.packages('gridExtra')
#LIBRERIAS----------------------------------------------------------------------
library(ggplot2)
library(qqplotr)
library(corrplot)
library(outliers)
library(dplyr)
library(GGally)
library(factoextra)
library(mice)
library(gridExtra)
library(rpart)
library(rpart.plot)
library(caret)
library(tidyverse)
library(ROCR)
library(Metrics)
library(yardstick)
library(flextable)
library(mlbench)
#-------------------------------------------------------------------------------
diamantes<-read.csv('Directorio/precio_diadm.csv', sep=',', header = T)
names(diamantes)<-c('Peso','Talla','Color','Pureza','Profundidad','Ancho','Precio','X','Y','Z')
diamantes
str(diamantes)#Observamos los tipos de variables por columnas
summary(diamantes)#obtenemos un sumario respecto al estado de las columas establecidas
#PARA NUESTROS DATROS NO EXISTE EN LAS COLUMNAS VALORES FALTANTES.

tablaCorte<-table(diamantes$Corte);tablaCorte#Podemos obtener la cantidad de cada uno de los cortes.

tablaColor<-table(diamantes$Color);tablaColor

tablaClaridad<-table(diamantes$Claridad);tablaClaridad

#Seria importante econtrar una relación entre el precio del diamantes y alguna de sus caracteristicas.
#En primera instancia obtener los estadisticos de la comuna 'Precio' de los diamantes.

diamantes$Talla<-as.factor(diamantes$Talla)
diamantes$Color<-as.factor(diamantes$Color)
diamantes$Pureza<-as.factor(diamantes$Pureza)
summary(diamantes)#Observamos los tipos de variables por columnas
