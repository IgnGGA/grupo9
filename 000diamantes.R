rm(list=ls())
library(ggplot2)
library(qqplotr)
library(corrplot)
library(outliers)
library(dplyr)
library(GGally)
library(factoextra)
library(mice)
library(gridExtra)
library(rpart)
library(rpart.plot)
library(caret)
library(tidyverse)
library(ROCR)
library(Metrics)
library(yardstick)
library(flextable)
library(mlbench)
library(neuralnet)
library(fastDummies)
library(tidyverse)
library(skimr)
library(corrr)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(ggpubr)
library(moderndive)
library(olsrr)
library(MASS)
library(car)
library(ggthemr)
#-------------------------------------------------------------------------------
diamantes<-read.csv('Directorio/precio_diadm.csv', sep=',', header = T)
diamonds$cut <- as.factor(diamonds$cut)
diamonds$color <- as.factor(diamonds$color)
diamonds$clarity <- as.factor(diamonds$clarity)

diamantes<-diamantes[!(diamantes$x==0.0),]
diamantes<-diamantes[!(diamantes$y==0.0),]
diamantes<-diamantes[!(diamantes$z==0.0),]

str(diamantes)
summary(diamantes)

smp_siz = floor(0.80*nrow(diamonds))
set.seed(123)
train_ind = sample(seq_len(nrow(diamonds)), size = smp_siz)
train = diamonds[train_ind,]
test = diamonds[-train_ind,] 

model_single <- lm(price ~ carat, data = train)
get_regression_summaries(model_single)

model_full <- lm(price ~ ., data = train)
get_regression_summaries(model_full)

model_bw <- step(model_full, direction = "backward", trace = FALSE)
model_fw <- step(model_single, scope = list(lower = model_single, upper = model_full), direction = "forward", trace = FALSE)
model_bo <- step(model_single, scope = list(lower = model_single, upper = model_full), direction = "both", trace = FALSE)

get_regression_summaries(model_bw)
get_regression_summaries(model_fw)
get_regression_summaries(model_bo)
#___________

summary(diamantes)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

maxmindf <- as.data.frame(lapply(diamantes, normalize))
 