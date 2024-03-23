rm(list=ls()) #Limpiar consola

library(qcc)
library(funModeling)

setwd("C:/Users/igngg/OneDrive - UNIVERSIDAD ANDRES BELLO/Documents/003_tercerAño_2024/05_mineriaDeDatos/Grupo_9") #Nuevo directorio
conjuntoDeDatos<-read.csv('Directorio/precio_diadm.csv')

dim(conjuntoDeDatos)
str(conjuntoDeDatos)
head(conjuntoDeDatos)

hist(conjuntoDeDatos$carat)