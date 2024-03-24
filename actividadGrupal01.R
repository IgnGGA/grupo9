rm(list=ls()) #Limpiar consola

library(qcc)
library(ggplot2)

setwd("C:/Users/igngg/OneDrive - UNIVERSIDAD ANDRES BELLO/Documents/003_tercerAño_2024/05_mineriaDeDatos/Grupo_9") #Nuevo directorio
conjuntoDeDatos<-read.csv('Directorio/precio_diadm.csv')

names(conjuntoDeDatos)<-c('Peso','Corte','Color','Claridad','Profundidad','Ancho','Precio','X','Y','Z')

dim(conjuntoDeDatos)
str(conjuntoDeDatos)
head(conjuntoDeDatos)

hist(conjuntoDeDatos$Peso,
     main='Histograma de Peso',
     xlab='Peso',
     ylab='Frecuencia')

tb.freq<-function(x){
  f_i<-as.vector(table(x))
  tf<-cbind(f_i)
  row.names(tf)<-names(table(x))
  return(tf)
}

tabPeso<-tb.freq(conjuntoDeDatos$Peso)
tabPeso
tabPrecio<-tb.freq(conjuntoDeDatos$Precio)
tabPrecio
tabCorte<-tb.freq(conjuntoDeDatos$Corte)
tabCorte
graphPeso<-barplot(tabPrecio,
                   main='Peso de diamantes',
                   xlab='Peso',
                   ylab='Frecuencia',
                   border='black')
