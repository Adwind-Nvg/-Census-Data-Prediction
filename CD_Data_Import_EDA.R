#Census Data Prediction
#Data import and EDA

#Import library
library(e1071)
library(tidyverse)
library(ggplot2)
library(ggExtra)
library(gridExtra)
library(dplyr)

##Import 'census' dataset
raw <- read.csv("C:/Users/Abram/Desktop/OFF/Census_Data/adult.csv")

#preview dataset
summary(raw)
head(raw)

#From the results above, we can tell that the existing dataset does not contains col names, thus, we want to ssign the col names here
names(raw)<-c('age','workclass','fnlwgt','education','education_num','marital_status','occupation','relationship','race','sex','capital_gain','capital_loss','hours_per_week','native_country', 'income')
head(raw)

#Since the variable fnlwgt is an observation multiplier, we want to modify the dataset
#We want to rescale the dataset by 10k rather than directly multiplying, since if then, the we would have over 6 billions obs
raw$fnlwgt <- sapply(raw$fnlwgt, function(x) x/10000)
raw <- expandRows(raw, "fnlwgt")
str(raw)

#After the multiplication, the obs number is 601547, we droped the multiplication col fnlwgt, thus, we have 14 cols here.
#Return a table with variable names & types
table_name <- names(raw)
table_ty <- data.frame(matrix(nrow=1, ncol=1))

colnames(table_ty) <- c('type')
for (i in table_name) {
  name <- typeof(raw[,i])
  table_ty <- table_ty %>%
    add_row(type = name)
}

table_ty <- table_ty[-1,]
type <- data.frame(table_name,table_ty)
names(type) <- c('variable', 'type')

#Table here:
type

#Return the table of statistics for specific cols
var <- c("age", "education_num", "capital_gain", "capital_loss", "hours_per_week")

calstats <- function(x) {
  miss <- sum(is.na(x))
  min <- round(min(x,na.rm=TRUE), 2)
  max <- round(max(x,na.rm = TRUE), 2)
  median <- round(median(x, na.rm = TRUE), 2)
  mean <- round(mean(x, na.rm = TRUE), 2)
  sd <- round(sd(x,na.rm = TRUE), 2)
  skew <- round(skewness(x,na.rm=TRUE), 2)
  kurt <- round(kurtosis(x,na.rm=TRUE), 2)
  result <- data.frame(miss, min, max, median, mean, sd, skew, kurt)
  
}

rslt <- data.frame(matrix(nrow=0, ncol=8))
colnames(rslt) <- c('miss', 'min', 'max', 'median', 'mean', 'sd', 'skew', 'kurt')

for (i in var) {
  result <- calstats(raw[,i])
  rslt <- rslt %>%
    add_row(result)
}

rownames(rslt) <- c("age", "education_num", "capital_gain", "capital_loss", "hours_per_week")

#table here
rslt

#Since for the col 'education_num', there're only 16 distinct values, based on the fact that the col is about the years of education for each observation, 16 could be defined as 'few distinct value', thus, we want to convert this col into categorical. We will change this col to character first for the convenience of graph and to factor right before the models building part.
#Convert datatype for col 'education_num'
raw$education_num <- as.character(raw$education_num)
summary(raw$education_num)

out <- c("age", "capital_gain", "capital_loss", "hours_per_week")
#Identify outliers using 'boxplot.stats'
var_1 <- c("age", "capital_gain", "capital_loss", "hours_per_week")
outliers <- c()
for (i in var_1) {
  outliers <- c(outliers,length(boxplot.stats(raw[,i])$out))
}

#summary(raw)

data.frame(var_1,outliers)

#Imputation for outliers:
##After we check the original dataset, we noticed that the max and min for col 'age' are 99 and 1, the outliers that boxplot generated are in range of [79,90], from real life perspectives, we do not recognize these numbers are outliers, since its not uncommon for humen live up to 90, thus, we want to exclude col 'age' in the following outliers imputations
##We want to direct impute value for col 'hours_per_week' since there're two ranges of outliers(set them to missing first may cause issues assigning values, detailed explanation seen below steps)
##We found out there should be a collinearity between col 'education' and 'education-num', thus, we intend to use 'education' instead of 'education_num' in the model. We do not impute values for the missings of col 'education_num'
##'Capital-gain' and 'Capital_loss': we decide not include these two cols since they have too many zero-values

#For hours_per_week, since the outliers detected by the 'boxplot.stats' contains over 162095 records, given the 601547 observations that the original dataset provides, the outliers detected occupies more than 27% of the original volume. Thus we want to manually replace the outliers, details see below:
#The outliers range is between[1,17] & [79,99], these numbers are somehow not extinct in real working scenarios, however, through boxplot we can say that these outliers presented a flat distribution compared to the box itsself, which will do harm to the models we are about to build, thus, we replaced outliers in [1,17] with the average of values in range [1,17] and [79,99] with the values average in range [79,99].
#unique(boxplot.stats(raw$hours_per_week)$out)
hpwo_c<-boxplot.stats(raw$hours_per_week)$out

raw$hours_per_week[raw$hours_per_week > 78] <- round(mean(Filter(function(x) any(x > 78), hpwo_c)))
raw$hours_per_week[raw$hours_per_week < 18] <- round(mean(Filter(function(x) any(x < 18), hpwo_c)))

summary(raw$hours_per_week)

#Return the table for the count of distince values for char vars
data_type <- data.frame(unlist(sapply(raw,class)))
colnames(data_type)[1] <- "Variable Type"

cha <- c()
for (i in rownames(data_type)) {
  if (data_type[i,] == "character") {
    cha <- c(cha, i)} 
  else {cha}
}

cnt <- c()
for (i in cha) {
  new_cnt <- length(unique(raw[,i]))
  cnt <- c(cnt, new_cnt)
}

all_char <- data.frame(var = cha, cnt_uni = cnt)

all_char

#Plot histogram for each of the numeric variables.
par(mfrow=c(1,2))
hist(raw$age, main="Histogram of Age", xlab="age", col = "#FF6666")
hist(raw$hours_per_week, main="Histogram of hours_per_week", xlab="hours_per_week", col = "#FF6666")

#Bar chart for each of the categorical variables
ggplot(raw, aes(x = workclass)) + geom_bar(fill = "#FF6666")
ggplot(raw, aes(x = education)) + geom_bar(fill = "#FF6666")
ggplot(raw, aes(x = marital_status)) + geom_bar(fill = "#FF6666")
ggplot(raw, aes(x = occupation)) + geom_bar(fill = "#FF6666")
ggplot(raw, aes(x = relationship)) + geom_bar(fill = "#FF6666")
ggplot(raw, aes(x = race)) + geom_bar(fill = "#FF6666")
ggplot(raw, aes(x = sex)) + geom_bar(fill = "#FF6666")
ggplot(raw, aes(x = native_country)) + geom_bar(fill = "#FF6666")
ggplot(raw, aes(x = income)) + geom_bar(fill = "#FF6666")
ggplot(raw, aes(x = education_num)) + geom_bar(fill = "#FF6666")

#Before we jump into the model part, we want to first modify our data types here:
raw$workclass <- as.factor(raw$workclass)
raw$education <- as.factor(raw$education)
raw$marital_status <- as.factor(raw$marital_status)
raw$occupation <- as.factor(raw$occupation)
raw$relationship <- as.factor(raw$relationship)
raw$race <- as.factor(raw$race)
raw$sex <- as.factor(raw$sex)
raw$native_country <- as.factor(raw$native_country)
raw$income <- as.factor(raw$income)
raw$education_num <- as.factor(raw$education_num)

#Output file
#write.csv(raw,"C:\\Users\\Abram\\Desktop\\raw.csv", row.names = FALSE)