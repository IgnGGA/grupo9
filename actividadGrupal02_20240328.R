rm(list=ls()) #Limpiar consola

library(ggplot2)

setwd("C:/Users/igngg/OneDrive - UNIVERSIDAD ANDRES BELLO/Documents/003_tercerAño_2024/05_mineriaDeDatos/Grupo_9") #Nuevo directorio

diamantes<-read.csv('Directorio/precio_diadm.csv', sep=',', header = T)
names(diamantes)<-c('Peso','Corte','Color','Claridad','Profundidad','Ancho','Precio','X','Y','Z')
diamantes
str(diamantes)#Observamos los tipos de variables por columnas
summary(diamantes)#obtenemos un sumario respecto al estado de las columas establecidas

tablaCorte<-table(diamantes$Corte)
tablaCorte#Podemos obtener la cantidad de cada uno de los cortes.

tablaColor<-table(diamantes$Color)
tablaColor

tablaClaridad<-table(diamantes$Claridad)
tablaClaridad
