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
  if (ksTeste>0.05){
    print("La distribuicion es es similar a la teorica (dist. Normal)")
  }
  else{
    print("Se rechaza que la distribucion sea teoricamente similar a una normal")
  }
}
#-------------------------------------------------------------------------------
setwd("C:/Users/igngg/OneDrive - UNIVERSIDAD ANDRES BELLO/Documents/003_tercerAño_2024/05_mineriaDeDatos/Grupo_9") #Nuevo directorio

diamantes<-read.csv('Directorio/precio_diadm.csv', sep=',', header = T)
names(diamantes)<-c('Peso','Corte','Color','Claridad','Profundidad','Ancho','Precio','X','Y','Z')
diamantes
str(diamantes)#Observamos los tipos de variables por columnas
summary(diamantes)#obtenemos un sumario respecto al estado de las columas establecidas
#PARA NUESTROS DATROS NO EXISTE EN LAS COLUMNAS VALORES FALTANTES.

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
largoPrc<-length(na.omit(Prc));largoPrc#Largo de la columna inspeccionada
#Al carecer de datos faltante, el largo es el mismo que el indicado por la dimension de la tabla original 'diamantes'
#tb.freq(Prc)#Para el precio no sirve una tabla de frecuencia... segun yo por la variabilidad y cantidad de datos que hay
c.ksTest(Prc)
