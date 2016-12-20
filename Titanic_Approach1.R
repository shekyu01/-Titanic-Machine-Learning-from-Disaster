# Step-1. Getting started

#Loading caret package
library("caret")
library('plyr'); 
library('dplyr') # data manipulation

#Loading training data
train <- read.csv("C:/Users/yudhisthirs/Desktop/DataCamp_Material/10.Projects/2. Titanic Data Set/train.csv",stringsAsFactors = T)
test <- read.csv("C:/Users/yudhisthirs/Desktop/DataCamp_Material/10.Projects/2. Titanic Data Set/test.csv",stringsAsFactors = T)


#Looking at the structure of caret package.
str(train)



# Step-2. Pre-processing using Caret

sum(is.na(train))
sum(is.na(test))

#full  <- dplyr::bind_rows(train, test) # bind training & test data


#Imputing missing values using KNN.Also centering and scaling numerical columns
preProcValues_train <- preProcess(train[,-(1:2)], method = c("knnImpute","center","scale"))
preProcValues_test <- preProcess(test[,-1], method = c("knnImpute","center","scale"))


library('RANN')
train_processed <- predict(preProcValues_train, train)
sum(is.na(train_processed))


test_processed <- predict(preProcValues_test, test)
sum(is.na(test_processed))

#Checking the structure of processed train file
str(train_processed)


#Converting sex variable to numeric
train_processed$Sex<-ifelse(train_processed$Sex=='male',1,2)
test_processed$Sex<-ifelse(test_processed$Sex=='male',1,2)


#Getting The Number of Levels of a Feature
nlevels(train_processed$Embarked)
nlevels(train_processed$Ticket)
nlevels(train_processed$Cabin)


#creating dummy variables using one hot encoding
#Converting every categorical variable to numerical using dummy variables
dmy <- dummyVars(" ~ .", data = train_processed,fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = train_processed))
str(train_transformed)


dmy <- dummyVars(" ~ .", data = test_processed,fullRank = T)
test_transformed <- data.frame(predict(dmy, newdata = test_processed))
str(test_transformed)


#Step-4. Feature selection using Caret

trainSet <- train_transformed
testSet <- test_transformed

#Feature selection using rfe in caret
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)
outcomeName<-'Survived'
predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]
Survive_Pred_Profile <- rfe(trainSet[,predictors], trainSet[,outcomeName],
                         rfeControl = control)
Survive_Pred_Profile

predictors<-c("Sex", "Pclass", "Age", "Fare", "SibSp")


# Step-5. Training models using Caret

model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm')
model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf')
model_glm<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm')