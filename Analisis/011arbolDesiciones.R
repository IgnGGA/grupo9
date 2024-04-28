ejemData<-diamantes
visdat::vis_dat(ejemData)#grafica de tipos de datos

set.seed(26)
selectrows <- sample(1:nrow(ejemData),round(0.80*nrow(ejemData)))
selectrows
dat.train <- ejemData[selectrows,]
dat.test <- ejemData[-selectrows,]

tree <- rpart(Precio ~., data = dat.train, 
              method = "anova",cp=0.001)
rpart.plot(tree,fallen.leaves = FALSE, cex=0.65)
bestcp2 <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
bestcp2  

ejemData2 <- prune(tree, cp = bestcp2)
rpart.plot(ejemData2, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)

ejemData2$variable.importance %>% 
  data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  rename(Overall = '.') %>%
  ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia")
  
dat.test$pred1<-predict(ejemData,dat.test)
dat.train$pred1<-predict(ejemData,dat.train)
val.ejemData<-med.reg(dat.test$Precio,dat.test$pred1)
val.ejemData_2<-med.reg(dat.train$Precio,dat.train$pred1)
