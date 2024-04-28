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