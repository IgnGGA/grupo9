ejemData<-diamantes
visdat::vis_dat(ejemData)#grafica de tipos de datos

set.seed(26)
selectrows <- sample(1:nrow(ejemData),round(0.80*nrow(ejemData)))
selectrows
dat.train <- ejemData[selectrows,]
dat.test <- ejemData[-selectrows,]

data1 <- rpart(Precio ~., data = dat.train, 
              method = "anova",cp=0.001)
rpart.plot(data1,fallen.leaves = FALSE, cex=0.65)
bestcp2 <- data1$cptable[which.min(data1$cptable[,"xerror"]),"CP"]
bestcp2  

data2 <- prune(data1, cp = bestcp2)
rpart.plot(data2, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)

data2$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia")

dat.test
dat.train
  
dat.test$pred1<-predict(data1,dat.test)
dat.train$pred1<-predict(data1,dat.train)
val.data11<-med.reg(dat.test$Precio,dat.test$pred1)
val.data12<-med.reg(dat.train$Precio,dat.train$pred1)

plot.pred1<-ggplot(dat.test,aes(x=Precio,y=pred1))+
  geom_point()+
  geom_line(aes(x=Precio, y=Precio),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("Modelo 1");plot.pred1

dat.test$pred2<-predict(data2,dat.test)
dat.train$pred2<-predict(data2,dat.train)
val.data21<-med.reg(dat.test$Precio,dat.test$pred2)
val.data22<-med.reg(dat.train$Precio,dat.train$pred2)

all.medR<-data.frame(Modelos=c("Modelo 1","Modelo 2"),
                     rbind(val.data11,val.data21))
all.medR%>% flextable()#Tabla comparativa de ambos modelos, pero aca son iguales

plot.pred2<-ggplot(dat.test,aes(x=Precio,y=Precio))+
  geom_point()+
  geom_line(aes(x=Precio, y=Precio),linetype="dashed",col=2)+
  labs(x = "Observaciones", y = "Predicciones")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)+ggtitle("Modelo 2");plot.pred2

grid.arrange(plot.pred1, plot.pred2, ncol = 2)
