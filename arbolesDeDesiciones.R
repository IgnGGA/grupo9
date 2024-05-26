#division de datos
set.seed(46)
#ARBOL DE DESIIONES 1
selectrows<-sample(1:nrow(diamantes),round(0.75*nrow(diamantes)))
dat.train<-diamantes[selectrows,]
dat.test<-diamantes[-selectrows,]

modelo1<-rpart(price ~.,data=dat.train, method = 'anova', cp=0.001)
rpart.plot(modelo1, fallen.leaves = F)

bestcp<-modelo1$cptable[which.min(modelo1$cptable[,'xerror']),'CP']
bestcp

modelo2<-prune(modelo1, cp=bestcp)
rpart.plot(modelo2, fallen.leaves = F)

#importancia de las varaibles

modelo2$variable.importance %>%
data.frame() %>%
rownames_to_column(var='Feature') %>%
rename(Overall='.') %>%
ggplot(aes(x = fct_reorder(Feature, Overall), y = Overall)) +
  geom_pointrange(aes(ymin = 0, ymax = Overall), color = "cadetblue", size = .3) +
  theme_minimal() +
  coord_flip() +
  labs(x = "", y = "", title = "Importancia")

med.reg<-function(obs,pred){
    e=obs-pred
    bias=mean(e)
    mse=mean((e)^2)
    mae=mean(abs(e))
    rmse=sqrt(mse)
    R2=1-(sum(e)^2/sum((obs-mean(obs))^2))
    medidas=data.frame(bias,mse,mae,rmse,R2)
    medidas
}

dat.test$pred1<-predict(modelo1, dat.test)
dat.train$pred1<-predict(modelo1, dat.train)
val.mod1<-med.reg(dat.test$price, dat.test$pred1)
val.mod1_2<-med.reg(dat.train$price, dat.train$pred1)

plot.pred1<-ggplot(dat.test, aes(x=price,y=pred1))+
    geom_point()+
    geom_line(aes(x=price, y=price),linetype='dashed',col=2)+
    theme(text=element_text(size=14))+
    theme_grey(base_size = 16)+ggtitle('Modelo 1');plot.pred1

dat.test$pred2<-predict(modelo2, dat.test)
dat.train$pred2<-predict(modelo2,dat.train)
val.mod2<-med.reg(dat.test$price,dat.test$pred2)
val.mod2_2<-med.reg(dat.train$price, dat.train$pred2)

all.medR<-data.frame(Modelos=c('Modelo 1', 'Modelo 2'),
                    rbind(val.mod1, val.mod2))
all.medR%>% flextable()

plot.pred2<-ggplot(dat.test,aes(x=price,y=pred2))+
    geom_point()+
    geom_line(aes(x=price, y=price), linetype='dashed',col=2)+
    labs(x='Observaciones', y='Precciones')+
    theme(text=element_text(size=14))+
    theme_gray(base_size=16)+ggtitle('Modelo 2');plot.pred2

grid.arrange(plot.pred1, plot.pred2, ncol = 2)

all.medR_1<-data.frame(Modelo1=c("Train","Test"),
                       rbind(val.mod1_2,val.mod1))

all.medR_2<-data.frame(Modelo2=c("Train","Test"),
                       rbind(val.mod2_2,val.mod2))

all.medR_1%>% flextable()
all.medR_2%>% flextable()
