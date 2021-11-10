#Census Data Prediction
#Decison Tree Predictive Model

#Import library
library(pROC)
library(ROCit)
library(caret)
library(regclass)
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

#Based on the training data building model
dc <- rpart(income~.,data=train_data1)

#This step first check the significance of different variables in the dc(model) and plot the decision Tree
varImp(dc)
#visualize_model(dc)
rpart.plot(dc, box.palette="RdBu", shadow.col="gray", nn=TRUE)

#This step check the roc statsitics as well as plot the roc curve
roc(test_data1$income,predict(dc,newdata=test_data1,type="prob")[,2])
plot(roc(test_data1$income,predict(dc,newdata=test_data1,type="prob")[,2]))

#This step we provide the accuracy, misclassification rate, true positive rate, false positive rate, specificity, precision, and prevalence statistics.
classifications_Tree <- predict(dc,newdata=test_data1,type='class')
matrix_tree <- confusionMatrix(classifications_Tree, test_data1$income,positive =' >50K') 

metrics_tree <- nb_cm
metrics_tree['accuracy',1] <- matrix_tree$overall[1]
metrics_tree['misclassification',1] <- 1 - matrix_tree$overall[1]
metrics_tree['true positive',1] <- 23160/(23160+10414)
metrics_tree['false positive',1] <- 20152/(20152+126739)
metrics_tree['specificity',1] <- matrix_tree$byClass[2]
metrics_tree['precision',1] <- matrix_tree$byClass[5]
metrics_tree['prevalence.',1] <- matrix_tree$byClass[8]
metrics_tree