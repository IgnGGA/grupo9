rm(list=ls()) #Limpiar consola
setwd("C:/Users/igngg/OneDrive - UNIVERSIDAD ANDRES BELLO/Documents/003_tercerAño_2024/05_mineriaDeDatos/Grupo_9/Directorio") #Nuevo directorio

conjuntoDeDatos<-read.csv('Directorio/precio_diadm.csv')

dim(conjuntoDeDatos)
head(conjuntoDeDatos)
str(conjuntoDeDatos)
