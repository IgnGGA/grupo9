Pso<-diamantes$Peso
promPso<-mean(Pso);promPso
mediaPso<-median(Pso);mediaPso
Q1Pso<-quantile(Pso, probs = 0.25)[[1]];Q1Pso
Q3Pso<-quantile(Pso, probs=0.75)[[1]];Q3Pso
varPso<-var(Pso);varPso
sdPso<-sd(Pso);sdPso
IQRPso<-IQR(Pso);IQRPso
coefAsimPso<-c.coefAsim(Pso);coefAsimPso
coefVarPso<-c.coefVar(Pso);coefVarPso
hist(Pso,
     main='Histograma de Peso',
     xlab='Peso',
     ylab='Frecuencia')
largoPrc<-length(na.omit(Pso));largoPrc
ggplot(data = diamantes[!is.na(diamantes$Peso),], mapping = aes(sample = Peso)) +
  stat_qq_line()+
  stat_qq_band()+
  stat_qq_point()+
  labs(x = "Q-Normal", y = "Q-Peso (ppb)")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16)

#sU COMPORTAMIENTO NO SE ASEMEJA AL DE UNA DISTRIBUICION NORMAL.

LSPso<-Q3Pso+1.5*IQRPso;LSPso
LIPso<-Q1Pso-1.5*IQRPso;LIPso
which(Pso>=LSPso)
Pso[Pso>=LSPso]
Pso[which(Pso>=LSPso)]
grubbs.test(Pso)
boxp_pso<-ggplot(diamantes[!is.na(Pso),], aes(y=Peso)) + 
  geom_boxplot(fill="yellow",varwidth = T)+
  labs(x = "Talla", y = "Peso (qt)")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16);boxp_pso
boxp_psoc<-ggplot(diamantes[!is.na(Pso),], aes(y=Peso, x=factor(Talla))) + 
  geom_boxplot(fill="lightyellow",varwidth = T)+
  labs(x = "Talla", y = "Peso (qt)")+
  theme(text = element_text(size=14))+
  theme_grey(base_size = 16);boxp_psoc
