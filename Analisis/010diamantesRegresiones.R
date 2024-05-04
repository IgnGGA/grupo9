#El documento se encuentra sin datos faltantes por lo tanto no requiere usar mice

diamantes<-diamantes[!(diamantes$X==0.0),]
diamantes<-diamantes[!(diamantes$Y==0.0),]
diamantes<-diamantes[!(diamantes$Z==0.0),]

D2=diamantes[,-c(2,3,4)]

diamond<-diamantes[,-c(2,3,4)]# al tadaframe le extraemos las columnas con datos cualitativos

pca_diamond <- prcomp(diamond,center=TRUE,scale = TRUE)
pca_diamond

res.var <- get_pca_var(pca_diamond)
corrplot(res.var$cos2, is.corr=FALSE)
pca_diamond$center
pca_diamond$scale
pca_diamond$rotation

summary(pca_diamond)
biplot(pca_diamond)

pca_diamond$x

VE <- pca_diamond$sdev^2
PVE <- VE / sum(VE)
round(PVE, 2)
cumsum(PVE)

plot(pca_diamond,type="l")

library(factoextra)
PVEp<- fviz_eig(pca_diamond)#,geom="bar")
PVEp

PVEa <- qplot(c(1:ncol(D2)), cumsum(PVE)) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("PVE-acumulado") + 
  # ggtitle("Cumulative Scree Plot") +
  ylim(0,1)

PVEa

grid.arrange(PVEp, PVEa, ncol = 2)

eig.val <- get_eigenvalue(pca_diamond)
eig.val

res.var <- get_pca_var(pca_diamond)
summary(pca_diamond)

fviz_pca_var(pca_diamond,col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", 
                                                            "#FC4E07"),repel = TRUE,axes = c(2,1))
library(corrplot)
corrplot(res.var$cos2, is.corr=FALSE)