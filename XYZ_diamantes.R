dX=diamantes$X
dY=diamantes$Y
dZ=diamantes$Z
#Desviaciones estandar
sdX=sd(dX);sdX
sdY=sd(dY);sdY
sdZ=sd(dZ);sdZ
#Coeficientes de asimetria
CoAsX=c.coefAsim(dX);CoAsX
CoAsY=c.coefAsim(dY);CoAsY
CoAsZ=c.coefAsim(dZ);CoAsZ
#Coeficientes de variación
CoVaX=c.coefVar(dX);CoVaX
CoVaY=c.coefVar(dY);CoVaY
CoVaZ=c.coefVar(dZ);CoVaZ

