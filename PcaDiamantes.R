diamonds<-diamantes[,-c(2,3,4)]
pcaD<-prcomp(diamonds,center=TRUE,scale=TRUE)
pcaD
res.var <- get_pca_var(pcaD)
corrplot(res.var$cos2, is.corr=FALSE)
