library(factoextra)
library(ks)

diamantes<-dummy_cols(diamantes, select_columns = c('cut', 'color', 'clarity'))
#sacar precio del conjunto de datos
#pasar a dummy caracteristicas
#Realizar PCA
#aplicar ks
#establecer rangos de precio para comparacion
#en caso de no generar buenos resultados, sin considerar vr. dummy

diamantesScale <- scale(diamantes) # Scaling the data
##ver matriz de distancia
distance <- get_dist(diamantesScale)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
#Determinar número óptimo de k
# Elbow method 
fviz_nbclust(diamantesScale, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

# Average silhouette
fviz_nbclust(diamantesScale, kmeans, method = "silhouette")

### Gap statistic
fviz_nbclust(diamantesScale, kmeans, method = "gap_stat")

#ajustar modelo
fit2<-kmeans(diamantesScale, 2)
table(fit2$cluster)
barplot(table(fit2$cluster),col=c(2,3))
fviz_cluster(fit2, data = diamantes)
#funModeling::freq(fit1$cluster)
#evaluar modelo
diamantes$clus<-as.vector(fit2$cluster)

ggplot(diamantes, aes(y=Murder, x=factor(clus))) + 
  geom_boxplot(fill="lightgreen",varwidth = T)+
  labs(x = "cluster", y = "Murder")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)

ggplot(diamantes, aes(y=UrbanPop, x=factor(clus))) + 
  geom_boxplot(fill="lightgreen",varwidth = T)+
  labs(x = "cluster", y = "UrbanPop")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)

ggplot(diamantes, aes(y=Assault, x=factor(clus))) + 
  geom_boxplot(fill="lightgreen",varwidth = T)+
  labs(x = "cluster", y = "Assault")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)