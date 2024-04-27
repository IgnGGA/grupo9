#Eliminador de datos atipicos

Q1<-quantile(diamantes$Precio, probs = 0,25)[[1]]
Q3<-quantile(diamantes$Precio, probs = 0,75)[[1]]
RIV=1.5*IQR(diamantes$Precio)
for (i in length(diamantes$Precio)){
  diamantes<-diamantes[!(diamantes$Precio>Q3+RIV),]
  diamantes<-diamantes[!(diamantes$Precio<Q1-RIV),]}
boxplot(diamantes$Z)


diamantes<-diamantes[!(diamantes$X==0.0),]
diamantes<-diamantes[!(diamantes$Y==0.0),]
diamantes<-diamantes[!(diamantes$Z==0.0),]
summary(diamantes)
