set.seed(123)
diamantes_01<-dummy_cols(diamantes, select_columns = c('Talla', 'Pureza', 'Color'))
diamantes_01<-diamantes_01[,-c(2,3,4)]

selectrows <- sample(1:nrow(diamantes_01),round(0.75*nrow(diamantes_01)))
dat.train <- diamantes_01[selectrows,]
dat.test <- diamantes_01[-selectrows,]

##Regresión Lineal múltiple
mod1<-lm(Precio~.,data = dat.train)
#inferencia de los coeficientes
coefic.lm<-summary(mod1)$coefficients
coefic.lm<-data.frame(Variables=row.names(coefic.lm),round(coefic.lm,4))
colnames(coefic.lm)<-c("Variables","Estimación","Std.Error","t.value","P-valor")
coefic.lm %>% flextable()

###Seleccionar el mejor modelo
mod2.lm <- step(mod1, direction="backward",trace = 0) #trace=1 muestra los pasos que realiza.
#inferencia de los coeficientes
coefic.lm2<-summary(mod2.lm)$coefficients
coefic.lm2<-data.frame(Variables=row.names(coefic.lm2),round(coefic.lm2,4))
colnames(coefic.lm2)<-c("Variables","Estimación","Std.Error","t.value","P-valor")
coefic.lm2 %>% flextable()

####Regresión con redes neuronales
library(neuralnet)
#normalizar los datos
maxs <- apply(diamantes_01, 2, max) 
mins <- apply(diamantes_01, 2, min)
scaled <- as.data.frame(scale(diamantes_01, center = mins, scale = maxs - mins))
dtrain.scal <- scaled[selectrows,]
dtest.scal<- scaled[-selectrows,]
#ajustar modelo NN 1
names(diamantes_01)=make.names(names(diamantes_01))
mod.regnn1 <- neuralnet(Precio ~.,data=dtrain.scal,hidden=c(5,3),linear.output=T)
plot(mod.regnn1,rep="best")

#ajustar modelo NN 2
mod.regnn2 <- neuralnet(Precio ~.,data=dtrain.scal[,c(row.names(coefic.lm2)[-1],"Precio")],hidden=c(5,3),linear.output=T)
plot(mod.regnn2,rep="best")

##Predicción
med.reg<-function(obs,pred){
  e = obs-pred
  bias = mean(e)
  mse = mean((e)^2)
  mae = mean(abs(e))
  rmse = sqrt(mse)
  R2 = 1-(sum((e)^2)/sum((obs-mean(obs))^2))
  medidas = data.frame(bias,mse,mae,rmse,R2)
  medidas
}

#predicción lm 1
dat.test$pred1<-predict(mod1,dat.test)
val.mod1<-med.reg(dat.test$Precio,dat.test$pred1)

#predicción lm 2
dat.test$pred2<-predict(mod2.lm,dat.test)
val.mod2<-med.reg(dat.test$Precio,dat.test$pred2)

#predicción nn1
pred.nn1 <- compute(mod.regnn1,dtest.scal[,1:13])
pred.nn1 <- pred.nn1$net.result*(max(diamantes_01$Precio)-min(diamantes_01$Precio))+min(diamantes_01$Precio)
val.mod.nn1<-med.reg(dat.test$Precio,pred.nn1)

#predicción nn1
pred.nn2 <- compute(mod.regnn2,dtest.scal[,1:13])
pred.nn2 <- pred.nn2$net.result*(max(diamantes_01$Precio)-min(diamantes_01$Precio))+min(diamantes_01$Precio)
val.mod.nn2<-med.reg(dat.test$Precio,pred.nn2)

#medidas de desempeño de modelos
all.medR<-data.frame(Modelos=c("LM 1","LM 2","NN 1","NN 2"),
                     rbind(val.mod1,val.mod2,val.mod.nn1,val.mod.nn2))
all.medR%>% flextable()

##Gráficos de dispersión
plot.pred1<-ggplot(dat.test,aes(x=Precio,y=pred1))+
  geom_point()+
  geom_line(aes(x=Precio, y=Precio),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("LM 1")

plot.pred2<-ggplot(dat.test,aes(x=Precio,y=pred2))+
  geom_point()+
  geom_line(aes(x=Precio, y=Precio),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("LM 2")

plot.pred3<-ggplot(dat.test,aes(x=Precio,y=pred.nn1))+
  geom_point()+
  geom_line(aes(x=Precio, y=Precio),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("NN 1")

plot.pred4<-ggplot(dat.test,aes(x=Precio,y=pred.nn2))+
  geom_point()+
  geom_line(aes(x=Precio, y=Precio),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("NN 2")

grid.arrange(plot.pred1, plot.pred2, 
             plot.pred3, plot.pred4,
             ncol = 2)