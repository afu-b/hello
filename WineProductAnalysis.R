# Fin211-Big Data
# Classification Project
# Red Wine Quality 

# Background
# Multi-class classification, numeric inputs
# Dataset Description: https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009

# Load Libraries
library(fmsb)
library(readr)
library(ellipse)
library(ggplot2)
library(lattice)
library(dplyr)
library(recipes)
library(caret)

# 1. Load Data
# attach the dataset to the environment
wine <- read.csv("Desktop/211 project1/winequality-red.csv")
# rename the dataset
dataset <- wine
# classfication of quality
dataset[,'quality.class'] <- cut(dataset$quality,breaks=c(-Inf,4,6,Inf),labels=c('low','medium','high'))
dataset <- dataset[,-12]

# 2. Split out validation dataset
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$quality.class, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]

# 3. Summarize Dataset
# dimensions of dataset 
dim(dataset)
# [1] 1281   12
# list types for each attribute
sapply(dataset, class)
# numeric and factor
# take a peek at the first 5 rows of the data
head(dataset)
# list the levels for the class
levels(dataset$quality.class)
# split input and output
x <- dataset[,1:11]
y <- dataset[,12]
# summarize the class distribution
percentage <- prop.table(table(dataset$quality.class)) * 100
cbind(freq=table(dataset$quality.class), percentage=percentage)
# summarize attribute distributions
summary(dataset)

# 4. Visualize Dataset
# a) Univariate
# boxplots for numeric
par(mfrow=c(2,6))
for(i in 1:11) {
  boxplot(x[,i], main=names(dataset)[i])
}
# barplot for class breakdown
par(mfrow=c(1,1))
bar = plot(y,density=c(30,20,10),angle=c(0,45,90),col="brown",ylim=c(0,1200))
text(bar[1],150, paste(round(percentage[1],2),"%",sep="") ,cex=1) 
text(bar[2],1150, paste(round(percentage[2],2),"%",sep="") ,cex=1) 
text(bar[3],260, paste(round(percentage[3],2),"%",sep="") ,cex=1) 
# b) Multivariate
# scatterplot matrix
featurePlot(x=x[1:5], y=y, plot="ellipse") 
# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box",scales=scales,layout=c(3,4))
# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales,layout=c(3,4))
# radar chart
data = aggregate(x = dataset[, 1:11], by =list(dataset$quality.class), FUN = mean)[,-1]
rownames(data) = c("low","medium","high")
maxdata = lapply(data,max)
mindata = lapply(data,min)
data = rbind(maxdata,mindata,data)
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart( data , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)
legend(x=1.2, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)



# 5. Evaluate Algorithms
dataset[,1:11] <- scale(dataset[,1:11])
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
# a) linear algorithms
set.seed(7)
fit.lda <- train(as.factor(quality.class)~., data=dataset, method="lda", metric=metric, trControl=control,preProc=c("center", "scale", "BoxCox"),na.action = na.omit)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(as.factor(quality.class)~., data=dataset, method="rpart", metric=metric, trControl=control,preProc=c("center", "scale", "BoxCox"),na.action = na.omit)
# kNN
set.seed(7)
fit.knn <- train(as.factor(quality.class)~., data=dataset, method="knn", metric=metric, trControl=control,preProc=c("center", "scale", "BoxCox"),na.action = na.omit)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(as.factor(quality.class)~., data=dataset, method="svmRadial", metric=metric, trControl=control,preProc=c("center", "scale", "BoxCox"),na.action = na.omit)
# Random Forest
set.seed(7)
fit.rf <- train(as.factor(quality.class)~., data=dataset, method="rf", metric=metric, trControl=control,preProc=c("center", "scale", "BoxCox"),na.action = na.omit)
# d) compare algorithms
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
dotplot(results)
# summarize Best Model
print(fit.rf)

# 6. Estimate Skill on Validation Dataset
set.seed(7)
pre_rf <- predict(fit.rf, newdata=validation)
confusionMatrix(pre_rf, validation$quality.class)

# 7. Improve accurancy by esembling (majority vote)
pre_svm <- predict(fit.svm, newdata=validation)
pre_cart <- predict(fit.cart, newdata=validation)
fit_majority <- rep(0,length(pre_rf))
for(i in 1: length(pre_rf)){
  if(pre_svm[i]==pre_cart[i] && pre_rf[i]!=pre_cart[i])
    fit_majority[i]=as.character(pre_cart[i]) else fit_majority[i]=as.character(pre_rf[i])
}
confusionMatrix(fit_majority, validation$quality.class)
# The result is not good for esembling.


