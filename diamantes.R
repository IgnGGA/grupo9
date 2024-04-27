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
#FUNCIONES----------------------------------------------------------------------
c.coefAsim<-function(x){#Funcion de asimetria
  a=mean((x-mean(x,na.rm=T))^3,na.rm=T)
  coefAsim=a/(sd(x,na.rm = T)^3)
  coefAsim
}
c.coefVar<-function(x){#Funcion coeficiente de variabilidad
  coefVar=sd(x,na.rm=T)/mean(x,na.rn=T)
  coefVar
}
tb.freq<-function(x){#Tabla de frecuencias
  Fa<-as.vector(table(x))#Frecuencia Absoluta
  Faa<-cumsum(Fa)#Frecuencia Acumulada
  FR<-Fa/length(x)#Frecuencia Relativa
  FRA<-Faa/length(x)#Frecuencia Relativa Acumulada
  tf<-cbind(Fa, Faa, FR, FRA)
  row.names(tf)<-names(table(x))
  return(tf)
}
c.ksTest<-function(x){#Se realiza una funcion para realizar KS Test's directos a la fraccion de tabla a consulta.
  ksTest<-ks.test(x,"pnorm",mean=mean(x,na.rm = T),sd=sd(x,na.rm=T))
  ksTest
}
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
