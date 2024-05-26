set.seed(123)
diamantes<-diamantes[,-c(2,3,4)]


seleccionarFilas <- sample(1:nrow(diamantes),round(0.75*nrow(diamantes)))
datos.entranmieto <- diamantes[seleccionarFilas,]
datos.prueba <- diamantes[-seleccionarFilas,]

mod1<-lm(price~.,data = datos.entranmieto)

coefic.lm<-summary(mod1)$coefficients
coefic.lm<-data.frame(Variables=row.names(coefic.lm),round(coefic.lm,4))
colnames(coefic.lm)<-c("Variables","Estimación","Std.Error","t.value","P-valor")
coefic.lm %>% flextable()

mod2.lm <- step(mod1, direction="backward",trace = 0)

coefic.lm2<-summary(mod2.lm)$coefficients
coefic.lm2<-data.frame(Variables=row.names(coefic.lm2),round(coefic.lm2,4))
colnames(coefic.lm2)<-c("Variables","Estimación","Std.Error","t.value","P-valor")
coefic.lm2 %>% flextable()

maximos <- apply(diamantes, 2, max) 
minimos <- apply(diamantes, 2, min)
escala <- as.data.frame(scale(diamantes, center = minimos, scale = maximos - minimos))
dEntrenamiento.escala <- escala[seleccionarFilas,]
dPruebas.escala<- escala[-seleccionarFilas,]

mod.regnn1 <- neuralnet(price ~., data=dEntrenamiento.escala,hidden=c(5,3),stepmax = 10000000,linear.output=T)
plot(mod.regnn1,rep="best")

mod.regnn2 <- neuralnet(price ~.,data=dEntrenamiento.escala[,c(row.names(coefic.lm2)[-1],"price")],hidden=c(5,3),stepmax = 10000000,linear.output=T)
plot(mod.regnn2,rep="best")



