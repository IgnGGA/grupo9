#El documento se encuentra sin datos faltantes por lo tanto no requiere usar mice
diamond<-diamantes[,-c(2,3,4)]# al tadaframe le extraemos las columnas con datos cualitativos

pca_diamond <- prcomp(diamond,center=TRUE,scale = TRUE)
pca_diamond
summary(pca_diamond)

res.var <- get_pca_var(pca_diamond)
corrplot(res.var$cos2, is.corr=FALSE)
