rm(list=ls())
library(ggplot2)
library(qqplotr)
library(corrplot)
library(outliers)
library(dplyr)
library(GGally)
library(factoextra)
library(mice)
library(gridExtra)
library(rpart)
library(rpart.plot)
library(caret)
library(tidyverse)
library(ROCR)
library(Metrics)
library(yardstick)
library(flextable)
library(mlbench)
library(neuralnet)
library(fastDummies)
library(tidyverse)
library(skimr)
library(corrr)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(ggpubr)
library(moderndive)
library(olsrr)
library(MASS)
library(car)
library(FNN) 
library(funModeling) 
library(klaR) 
library(NeuralNetTools) 
library(rattle)
#-------------------------------------------------------------------------------
diamantes<-read.csv('Directorio/precio_diadm.csv', sep=',', header = T)
dNeural<-read.csv('Directorio/precio_diadm.csv', sep=',', header = T)

diamantes<-diamantes[!(diamantes$x==0.0),]
diamantes<-diamantes[!(diamantes$y==0.0),]
diamantes<-diamantes[!(diamantes$z==0.0),]
dNeural<-dNeural[!(dNeural$x==0.0),]
dNeural<-dNeural[!(dNeural$y==0.0),]
dNeural<-dNeural[!(dNeural$z==0.0),]

diamonds$cut <- as.factor(diamonds$cut)
diamonds$color <- as.factor(diamonds$color)
diamonds$clarity <- as.factor(diamonds$clarity)

str(diamantes)
summary(diamantes)

#organizar datos para entrenamiento y prueba
#Cut
dNeural$cut <- factor(dNeural$cut, levels = c("Ideal", "Premium", "Very Good", "Good", "Fair"))
dNeural$cut <- as.numeric(dNeural$cut)
attr(dNeural$cut, "levels") <- NULL

#Color
dNeural$color <- factor(dNeural$color, levels = c("D", "E", "F", "G", "H", "I", "J"))
dNeural$color <- as.numeric(dNeural$color)
attr(dNeural$color, "levels") <- NULL

#Clarity
dNeural$clarity <- factor(dNeural$clarity, levels = c("IF", "VVS1", "VVS2", "VS1", "VS2", "SI1", "SI2", "I1"))
dNeural$clarity <- as.numeric(dNeural$clarity)
attr(dNeural$clarity, "levels") <- NULL

selectrows <- sample(1:nrow(dNeural),round(0.75*nrow(dNeural)))
dat.train <- dNeural[selectrows,]
dat.test <- dNeural[-selectrows,]

selectrows <- sample(1:nrow(dNeural),round(0.80*nrow(dNeural)))
dat.train <- dNeural[selectrows,]
dat.test <- dNeural[-selectrows,]

X_train = dat.train[, !names(dat.train) %in% c("price")]
X_test = dat.test[, !names(dat.test) %in% c("price")]
y_train = dat.train[,"price"]
y_test = dat.test$price

kprob<-c(5:20)
pred_y<-matrix(0,nrow = length(y_test),ncol=length(kprob))
for (i in 1:length(kprob)) {
  pred_y[,i] = knn.reg(train = scale(X_train), test = scale(X_test), 
                       y = y_train, k = kprob[i])$pred
}
maeK<-apply(pred_y, c(2), function(x) MAE(y_test, x))
plot(kprob,maeK,type = "o")

pred = knn.reg(train = scale(X_train), test = scale(X_test), 
               y = y_train, k = 5)$pred

bias=mean(y_test - pred) #modelo subestima
mse = mean((y_test - pred)^2)
mae = caret::MAE(y_test, pred)
rmse = sqrt(mse)
c(bias,mse,mae,rmse)
cor(y_test,pred)

apply(pred_y, c(2), function(x) cor(x,y_test))

plot(y_test,pred,pch=16)

set.seed(46)
selectrows <- sample(1:nrow(dNeural),round(0.80*nrow(dNeural)))
dat.train <- dNeural[selectrows,]
dat.test <- dNeural[-selectrows,]

# prepare resampling method
control <- trainControl(method="repeatedcv", number=20, repeats=6)

#Regresión Lineal Múltiple
st.time<-Sys.time()
fitR.lm <- train(price~., data=dat.train, method="lm", metric="RMSE", trControl=control)
end.time<- Sys.time()
tim.lm<-end.time-st.time

coefic.lm<-summary(fitR.lm)$coefficients
coefic.lm<-data.frame(Variables=row.names(coefic.lm),round(coefic.lm,4))
colnames(coefic.lm)<-c("Variables","Estimación","Std.Error","t.value","P-valor")
library(flextable)
coefic.lm %>% flextable()
#gráfico Q-Q
plot(fitR.lm$finalModel,which = 2,col=c("blue"))

#árbol de decisión
st.time<-Sys.time()
fitR.dt <- train(price~., data=dat.train, method="rpart", metric="RMSE", trControl=control,na.action=na.omit,tuneLength=10)
end.time<-Sys.time()
tim.dt<-end.time-st.time

fitR.dt$results
library(rattle)
fancyRpartPlot(fitR.dt$finalModel,tweak=1)
#redes neuronales
st.time<-Sys.time()
fitR.nn <- train(price~., data=dat.train,
                 method = "nnet",metric="RMSE",trControl = control,linout=TRUE,
                 preProcess=c("scale","center"),na.action = na.omit,trace=F,maxit = 2000,tunelength=9)
end.time<-Sys.time()
tim.nn<-end.time-st.time
ggplot(fitR.nn, highlight = TRUE)
library(NeuralNetTools)
old.par <- par(mar = c(bottom = 1, left = 2, top = 2, right = 3), xpd = NA)
plotnet(fitR.nn$finalModel)


##K-vecinos más cercanos
st.time<-Sys.time()
fitR.knn <- train(price~., data=dat.train, method="knn", metric="RMSE",
                  linout=TRUE, preProcess=c("scale","center"), trControl=control,na.action=na.omit)
end.time<-Sys.time()
tim.knn<-end.time-st.time

#tiempo cada algoritmo (agregar de cada estudiantes)
time.all<-data.frame(Algoritmos=c("LM","DT","NN","KNN"),
                     Tiempo=c(tim.lm,tim.dt,tim.nn,tim.knn))
library(flextable)
time.all%>% flextable()

##gráfico comparativo
list_reg<-list(lm=fitR.lm,dt=fitR.dt, nn=fitR.nn,knn=fitR.knn)
all_reg <- resamples(list_reg)
summary(all_reg)
bwplot(all_reg, layout = c(3, 1),scales="free",cex.axis = 1.5)

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

pred.lm<-predict(fitR.lm,dat.test)
val.lm<-med.reg(dat.test$price,pred.lm)

pred.dt<-predict(fitR.dt,dat.test)
val.dt<-med.reg(dat.test$price,pred.dt)

pred.nn<-predict(fitR.nn,dat.test)
val.nn<-med.reg(dat.test$price,pred.nn)

pred.knn<-predict(fitR.knn,dat.test)
val.knn<-med.reg(dat.test$price,pred.knn)

all.medR<-data.frame(Algoritmos=c("LM","DT","NN","KNN"),rbind(val.lm,val.dt,val.nn,val.knn))
all.medR%>% flextable()

##gráficos
plot.predlm<-ggplot(dat.test,aes(x=price,y=pred.lm))+
  geom_point()+
  geom_line(aes(x=price, y=price),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("LM")

plot.preddt<-ggplot(dat.test,aes(x=price,y=pred.dt))+
  geom_point()+
  geom_line(aes(x=price, y=price),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("DT")

plot.prednn<-ggplot(dat.test,aes(x=price,y=pred.nn))+
  geom_point()+
  geom_line(aes(x=price, y=price),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("NN")

plot.predknn<-ggplot(dat.test,aes(x=price,y=pred.knn))+
  geom_point()+
  geom_line(aes(x=price, y=price),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("KNN")

library(gridExtra) 
grid.arrange(plot.predlm, plot.preddt,plot.prednn,plot.predknn, ncol = 2)