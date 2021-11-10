#Census Data Prediction
#Navie Bayes Predictive Model

#Import library
library(e1071)
library(pROC)
library(ROCit)
library(naivebayes)
library(psych)
library(caret)

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

#This step we use naive_bayes function in naivebayes library to train the model
model_nb <- naive_bayes(income ~ ., data = train_data1) 

#Previw probs
plot(model_nb)

#This step we use the model_nb that we built in the last step to predict income(using the test dataset)
p_nb <- predict(model_nb,newdata=test_data1,type="prob")[,2]

#Return a table of tier probs
p_nb_df <-cbind(p_nb, test_data1)
p_nb_df_s <- p_nb_df[order(p_nb),]
tail(p_nb_df_s,10)

#This step we check the statistics of the model_nb and graph the ROC curve
roc(test_data1$income,p_nb)
plot(roc(test_data1$income,p_nb))

#This step we provide the accuracy, misclassification rate, true positive rate, false positive rate, specificity, precision, and prevalence statistics.
classifications <- predict(model_nb,newdata=test_data1)
confusionmatrix <- confusionMatrix(classifications,test_data1$income, positive = " >50K")
nb_cm <- data.frame(matrix(ncol = 1,nrow = 7))
rownames(nb_cm) <- c('accuracy', 'misclassification', 'true positive', 'false positive', 'specificity', 'precision' ,'prevalence.')

colnames(nb_cm) <- 'values'
nb_cm['accuracy',1] <- confusionmatrix$overall[1]
nb_cm['misclassification',1] <- 1 - confusionmatrix$overall[1]
nb_cm['true positive',1] <- 30041/(16903+30041)
nb_cm['false positive',1] <- 13271/(13271+120250)
nb_cm['specificity',1] <- confusionmatrix$byClass[2]
nb_cm['precision',1] <- confusionmatrix$byClass[5]
nb_cm['prevalence.',1] <- confusionmatrix$byClass[8]

nb_cm