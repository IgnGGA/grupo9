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

med.reg<-function(obs,pred){#Funcon util para obtener su predicción
  e = obs-pred
  bias = mean(e)
  mse = mean((e)^2)
  mae = mean(abs(e))
  rmse = sqrt(mse)
  R2 = 1-(sum((e)^2)/sum((obs-mean(obs))^2))
  medidas = data.frame(bias,mse,mae,rmse,R2)
  medidas
}

trans_minmax <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

To_Dummy=function(variable){
  names=unique(variable)
  dfaux=data.frame(aux=variable)
  auxnames=c()
  for(i in names){
    assign(paste0(i,"_DM"),ifelse(variable==i,1,0))
    auxnames=c(auxnames,paste0(i,"_DM"))
    dfaux=cbind.data.frame(dfaux,get(paste0(i,"_DM")))
  }
  dfaux=dfaux[,-1]
  colnames(dfaux)=auxnames
  summary(dfaux)
  return(dfaux)
}