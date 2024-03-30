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
c.ksTest(Prc)#Si el valor de 'p.value' es mayor a 0,05 se considera que la distribuicion respecto a una dist. Normal son similares
#en caso contrario, se puede inferir que la distribuicion de la columna de interes no se asemeja una distribuicion normal.
ggplot(data = diamantes[!is.na(diamantes$Precio),], mapping = aes(sample = Precio)) +
  stat_qq_line()+
  stat_qq_band()+
  stat_qq_point()+
  labs(x = "Q-Normal", y = "Q-Precio (ppb)")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)

#sU COMPORTAMIENTO NO SE ASEMEJA AL DE UNA DISTRIBUICION NORMAL.
#valores atipicos.
LSPrc<-Q3Prc+1.5*IQRPrc;LSPrc#Limite Superior de valores atipicos
LIPrc<-Q1Prc-1.5*IQRPrc;LIPrc#Limite inferior de valores atipicos
#Valores Atipicos Superiores
which(Prc>=LSPrc)
Prc[Prc>=LSPrc]
Prc[which(Prc>=LSPrc)]
#valores Atipicos Inferiores
which(Prc<=LIPrc)
Prc[Prc<=LIPrc]
Prc[which(Prc<=LIPrc)]

grubbs.test(Prc)#Test de Grubbs

boxp_prc<-ggplot(diamantes[!is.na(Prc),], aes(y=Precio)) + 
  geom_boxplot(fill="blue",varwidth = T)+
  labs(x = "Talla", y = "Precio (USD)")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16);boxp_prc

boxp_prcc<-ggplot(diamantes[!is.na(Prc),], aes(y=Precio, x=factor(Corte))) + 
  geom_boxplot(fill="lightblue",varwidth = T)+
  labs(x = "Talla", y = "Precio (USD)")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16);boxp_prcc
