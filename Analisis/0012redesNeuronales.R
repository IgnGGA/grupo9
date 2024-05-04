set.seed(123)
training.samples <- diamantes$Precio %>% 
  createDataPartition(p = 0.8, list = FALSE)
dat.train  <- diamantes[training.samples, ]
dat.test <- diamantes[-training.samples, ]

#regresion lineal muntiple
modo1<-lm(Precio~.,data =  dat.train)

str(dat.train)
dat.train_aux<-dat.train[,-c(2,3,4)]
str(dat.train[,c(2,3,4)])
table(dat.train$Talla)
table(dat.train$Color)
table(dat.train$Pureza)


dat.train$Talla<-as.character(dat.train$Talla)


names<-unique(dat.train$Talla)
Ieal_DM<-ifelse(dat.train$Talla==names[1],1,0)
Prim_DM<-ifelse(dat.train$Talla==names[2],1,0)

dat.train$Color<-as.factor(dat.train$Color)
dat.train$Pureza<-as.factor(dat.train$Pureza)
str(dat.train)

function DummyT(variable){
  names<-unique(variable)
  for(i in names){
    assign(paste0(i,"_DM"),ifelse(variable==i,1,0))
  }
  return(df)

}

regnn1 <- neuralnet(Precio ~.,data=dat.train,hidden=c(5,3),linear.output=T)
