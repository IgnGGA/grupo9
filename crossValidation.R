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

