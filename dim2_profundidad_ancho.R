Prof=diamantes$Profundidad
Ancho=diamantes$Ancho
meanProf=mean(Prof);meanProf
meanAncho=mean(Ancho);meanAncho
medProf=median(Prof);medProf
medAncho=median(Ancho);medAncho
#El promedio y la media de ambas variables se encuentran cercanas
#Esto nos da indicios que podria tener un comportamiento de distribuicion normal
varProf=var(Prof);varProf
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
