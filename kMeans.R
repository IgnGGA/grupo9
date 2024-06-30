library(factoextra)
library(ks)

diamantes_aux<-diamantes
names(diamantes)
diamantes<-diamantes[,-c(3,4,7)]
summary(diamantes)
names(diamantes)
table(diamantes$cut)
diamantes$cut[diamantes$cut%in%"Fair"]<-"1"
diamantes$cut[diamantes$cut%in%"Good"]<-"2"
diamantes$cut[diamantes$cut%in%"Very Good"]<-"3"
diamantes$cut[diamantes$cut%in%"Premium"]<-"4"
diamantes$cut[diamantes$cut%in%"Ideal"]<-"5"
diamantes$cut<-as.numeric(diamantes$cut)

#sacar precio del conjunto de datos
#pasar a dummy caracteristicas
#Realizar PCA
#aplicar ks
#establecer rangos de precio para comparacion
#en caso de no generar buenos resultados, sin considerar vr. dummy
pcaD<-prcomp(diamantes,center=TRUE,scale=TRUE)
summary(pcaD)
res.var <- get_pca_var(pcaD)
corrplot(res.var$cos2, is.corr=FALSE)

pcaD_ux=pcaD$x[,1:3]
summary(pcaD_ux)
#Determinar número óptimo de k
# Elbow method 
library(factoextra)
fviz_nbclust(pcaD_ux, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

# Average silhouette
fviz_nbclust(pcaD_ux, kmeans, method = "silhouette")

### Gap statistic
fviz_nbclust(pcaD_ux, kmeans, method = "gap_stat")

#ajustar modelo
fit2<-kmeans(pcaD_ux, 2)
table(fit2$cluster)
barplot(table(fit2$cluster),col=c(2,3))
fviz_cluster(fit2, data = diamantes)
#funModeling::freq(fit1$clus)
#evaluar modelo
diamantes_aux <- diamantes_aux[-selectrows,]
diamantes_aux$clus<-as.vector(fit2$cluster)
head(diamantes_aux)

ggplot(diamantes_aux, aes(y=price, x=factor(clus))) + 
  geom_boxplot(fill="lightgreen",varwidth = T)+
  labs(x = "clus", y = "price")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)

ggplot(diamantes_aux, aes(y=depth, x=factor(clus))) + 
  geom_boxplot(fill="lightgreen",varwidth = T)+
  labs(x = "clus", y = "depth")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)

ggplot(diamantes_aux, aes(y=table, x=factor(clus))) + 
  geom_boxplot(fill="lightgreen",varwidth = T)+
  labs(x = "clus", y = "table")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)
