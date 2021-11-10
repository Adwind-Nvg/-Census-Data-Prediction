#Census Data Prediction
#Navie Bayes Predictive Model

#Import library
library(e1071)
library(tidyverse)
library(dplyr)
library(pROC)
library(ROCit)
library(caret)
library(splitstackshape)
library(caTools)

#Import modified 'census' dataset
raw<- read.csv("C:/Users/Abram/Desktop/raw.csv")
df <- raw[,c("age","workclass","education","marital_status","occupation","relationship","race","sex","hours_per_week","income")]
str(df)

#This step we partition the data into a training set (70%) and a validation set (30%)
set.seed(1234)
split <- sample.split(df, SplitRatio = 0.7)
train_data1 <- subset(df, split == "TRUE")
test_data1 <- subset(df, split == "FALSE")

str(train_data1)

#This step based on the training data build the random forest model
model_rf <- randomForest(income~., data=train_data1, mtry=3,ntree=6) 
varImp(model_rf) 
#visualize_relationship(model_rf,interest="age",on=train_data1)

#ROC Statistics
roc(test_data1$income,predict(model_rf,newdata=test_data1,type="prob")[,2])
plot(roc(test_data1$income,predict(model_rf,newdata=test_data1,type="prob")[,2]))

#This step we provide the accuracy, misclassification rate, true positive rate, false positive rate, specificity, precision, and prevalence statistics.
classifications_rf <- predict(model_rf,newdata=test_data1,type='class') 
matrix_rf <- confusionMatrix(classifications_rf, test_data1$income,positive=' >50K')

metrics_rf <- nb_cm
metrics_rf['accuracy',1] <- matrix_rf$overall[1]
metrics_rf['misclassification',1] <- 1 - matrix_rf$overall[1]
metrics_rf['true positive',1] <- 36250/(4467+36250)
metrics_rf['false positive',1] <- 7062/(7062+132686)
metrics_rf['specificity',1] <- matrix_rf$byClass[2]
metrics_rf['precision',1] <- matrix_rf$byClass[5]
metrics_rf['prevalence.',1] <- matrix_rf$byClass[8]
metrics_rf