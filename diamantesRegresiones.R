
#Los datos a trabajar se encuentra completos para tratar
#imp_dt<-mice(diamantes,m=1,seed=46)
#imp_dt$imp$Peso

str(diamantes)
diamond<-diamantes[,-c(2,3,4)]

pca_diamond <- prcomp(diamond,center=TRUE,scale = TRUE)
pca_diamond
summary(pca_diamond)

res.var <- get_pca_var(pca_diamond)
corrplot(res.var$cos2, is.corr=FALSE)
