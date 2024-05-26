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