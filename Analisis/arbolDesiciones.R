ejemData<-diamantes
visdat::vis_dat(ejemData)#grafica de tipos de datos

set.seed(26)
selectrows <- sample(1:nrow(ejemData),round(0.80*nrow(ejemData)))
selectrows
dat.train <- ejemData[selectrows,]
dat.test <- ejemData[-selectrows,]
library(rpart)
tree <- rpart(Profundidad ~., data = dat.train, 
              method = "anova",cp=0.001)
rpart.plot(tree,fallen.leaves = FALSE, cex=0.65)
table(pred=predict(tree, type = "class"),obs=train.data$diabetes)