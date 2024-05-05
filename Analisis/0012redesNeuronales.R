To_Dummy(diamantes$Talla)
To_Dummy(diamantes$Color)
To_Dummy(diamantes$Pureza)

set.seed(123)
training.samples <- diamantes$Precio %>% 
  createDataPartition(p = 0.8, list = FALSE)
dat.train  <- diamantes[training.samples, ]
dat.test <- diamantes[-training.samples, ]

maxs <- apply(diamantes, 2, max) 
mins <- apply(diamantes, 2, min)
scaled <- as.data.frame(scale(diamantes, center = mins, scale = maxs - mins))
dtrain.scal <- scaled[selectrows,]
dtest.scal<- scaled[-selectrows,]

regnn1 <- neuralnet(Precio ~.,data=dat.train,hidden=c(7),linear.output=T)
