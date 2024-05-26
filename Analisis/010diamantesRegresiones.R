#Normalizar los datos
as.factor(diamantes$cut)
as.factor(diamantes$clarity)
scaleddata<-scale(diamantes)