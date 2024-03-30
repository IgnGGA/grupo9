ggplot(diamantes,aes(x=Precio,y=Peso))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  labs(x = "Precio", y = "Peso")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)
#Segun yo, la correolacion existente no tiene un comportamiento lineal, aun que si es ascendente...

##Coeficientes de correlación
#covarianza
cov(Pso,Prc)#La covarianza es positiva, por lo tanto hay una dependencia directa 
#correlación de Pearson
cor(Pso,Prc)#La correlacion es cercano a 1, por lo tanto, si hay una correlacion entre las variables 
#Ho:cor=0 H1:cor<>0
cor.test(Pso,Prc)
#correlación de Spearman automático
cor(Pso,Prc,method = "pearson")

cor.test(Pso,Prc,method = "pearson")
##Tau de kendall
cor(Pso,Prc,method = "kendall")#podriamos decir que existe una tendencia a la correlacion entre ambras variables.


ggcorr(diamantes, nbreaks=10, palette="Set3", label=TRUE, label_size=5, size=4, legend.size=10)
