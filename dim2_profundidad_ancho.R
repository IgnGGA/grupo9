Prof=diamantes$Profundidad
Ancho=diamantes$Ancho
meanProf=mean(Prof);meanProf
meanAncho=mean(Ancho);meanAncho
medProf=median(Prof);medProf
medAncho=median(Ancho);medAncho
#El promedio y la media de ambas variables se encuentran cercanas
#Esto nos da indicios que podria tener un comportamiento de distribuicion normal
varProf=var(Prof);varProf#Variabilidad de datos respecto a la media
varAncho=var(Ancho);varAncho
sdProf=sd(Prof);sdProf
sdAncho=sd(Ancho);sdAncho
hist(Prof,
     main='Histograma de Profundidad',
     xlab='Profundidad',
     ylab='Frecuencia')
hist(Ancho,
     main='Histograma de Ancho',
     xlab='Ancho',
     ylab='Frecuencia')
coefAsimProf<-c.coefAsim(Prof);coefAsimProf#Si es 0 es simetrico, si es 1 o -1 es asimetrico positivo o negativo
coefAsimAncho<-c.coefAsim(Ancho);coefAsimAncho
coefVarProf<-c.coefVar(Prof);coefVarProf#Tenemos datos que tienen una relacion entre la desviacion estandar y la media es INFIMA
coefVarAncho<-c.coefVar(Ancho);coefVarAncho
