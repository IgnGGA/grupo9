rm(list=ls()) #Limpiar consola
#LIBRERIAS----------------------------------------------------------------------
library(ggplot2)
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

setwd("C:/Users/igngg/OneDrive - UNIVERSIDAD ANDRES BELLO/Documents/003_tercerAño_2024/05_mineriaDeDatos/Grupo_9") #Nuevo directorio

diamantes<-read.csv('Directorio/precio_diadm.csv', sep=',', header = T)
names(diamantes)<-c('Peso','Corte','Color','Claridad','Profundidad','Ancho','Precio','X','Y','Z')
diamantes
str(diamantes)#Observamos los tipos de variables por columnas
summary(diamantes)#obtenemos un sumario respecto al estado de las columas establecidas

tablaCorte<-table(diamantes$Corte);tablaCorte#Podemos obtener la cantidad de cada uno de los cortes.

tablaColor<-table(diamantes$Color);tablaColor

tablaClaridad<-table(diamantes$Claridad);tablaClaridad

#Seria importante econtrar una relación entre el precio del diamantes y alguna de sus caracteristicas.
#En primera instancia obtener los estadisticos de la comuna 'Precio' de los diamantes.

#Estadisticos 'Precio' (promPrc, mediaPrc, etc...)
Prc<-diamantes$Precio
promPrc<-mean(Prc);promPrc
mediaPrc<-median(Prc);mediaPrc#Mediana
Q1Prc<-quantile(Prc, probs = 0.25)[[1]];Q1Prc#Primer quintil
Q3Prc<-quantile(Prc, probs=0.75)[[1]];Q3Prc#Tercer Quintil
varPrc<-var(Prc);varPrc#Varianza
sdPrc<-sd(Prc);sdPrc#Desviacion estarndar
IQRPrc<-IQR(Prc);IQRPrc#Rango inter quintil
coefAsimPrc<-c.coefAsim(Prc);coefAsimPrc#Coeficiente de asimetria del precio, la cual es mayor que 0 indicando que es asimetrico
coefVarPrc<-c.coefVar(Prc);coefVarPrc#Coeficiente de ariabilidad
hist(Prc,
     main='Histograma de Precio',
     xlab='Precio',
     ylab='Frecuencia')#Histograma del precio, se observa que tiene un comportamiento atipico :s

