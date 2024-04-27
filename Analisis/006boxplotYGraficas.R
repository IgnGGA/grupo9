peso=diamantes$Peso
precio=diamantes$Precio
profun=diamantes$Profundidad
ancho=diamantes$Ancho
diamX=diamantes$X
diamY=diamantes$Y
diamZ=diamantes$Z
#___________________________________
c.ksTest(peso)#D = 0,12274 - p-value < 2.2e-16
c.ksTest(precio)#D = 0,18467 - p-value < 2.2e-16
c.ksTest(profun)#D = 0,075871 - p-value < 2.2e-16
c.ksTest(ancho)#D = 0,13225 - p-value < 2.2e-16
c.ksTest(diamX)#D = 0,093545 - p-value < 2.2e-16
c.ksTest(diamY)#D = 0,088528 - p-value < 2.2e-16
c.ksTest(diamZ)#D = 0,089273 - p-value < 2.2e-16
#___________________________________
boxplot(diamZ,
       outlty = 0,  # Tipo de línea
       outlwd = 1,  # Ancho de línea
       outpch = 23, # Símbolo pch para los atípicos
       outcex = 2,  # Tamaño de los atípicos
       outcol = 5,  # Color
       outbg = 4,   # Color de fondo (pch 21 a 25)
       medlty = 2,  # Tipo de línea de la mediana
       medlwd = 2,  # Ancho de línea de la mediana
       medpch = 21, # Símbolo pch
       medcex = 2,  # Tamaño del símbolo
       medcol = 5,  # Color de la línea
       medbg = 4)   # Color del pch (21 a 25)

boxplot(diamantes$talla, data = diamantes$Precio, col = "white")

boxp_prcc<-ggplot(diamantes[!is.na(diamZ),], aes(y=diamZ, x=factor(Color))) + 
  geom_boxplot(fill="purple",varwidth = T)+
  labs(x = "Color", y = "Z")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16);boxp_prcc

diamantes=diamantes[
  diamantes$X !=0 &
  diamantes$Y !=0 &
  diamantes$Z !=0,]
summary(diamantes)

grubbs.test(peso)
grubbs.test(precio)
grubbs.test(peso, type=11,opposite=FALSE,two.side=FALSE)
largo<-length(na.omit(diamX));largo

meanVar=mean(diamantes$Z);meanVar
for (i in 1:length(diamantes$Z)){
  if (diamantes$Z[i]==0){
    diamantes$Z[i]<-meanVar
  }
}
summary(diamantes)
