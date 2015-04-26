# Solving the Kaggle Titanic Problem Using Boosting and Decision Trees
# In Class Activity 2
# Goal: Predict survival of passengers

library(caret)              # createDataPartition
library(Hmisc)              # impute
library(rpart)              # rpart
library(doMC)
registerDoMC(cores = 6)

data <- read.csv('~/Desktop/CSCI 183 Data Science/Class Activity 2/train.csv')

# Create test and train data sets
trainIndices <- createDataPartition(y = data$Survived, times = 1, p = .8, list = FALSE)
train <- data[trainIndices,]
test <- data[-trainIndices,]


# Imputed NA values with the mean
imputedTrain <- train
imputedTrain$Age <- impute(train$Age, fun = mean)
sapply(imputedTrain, function (x) sum(is.na(x)))

imputedTest <- test
imputedTest$Age <- impute(test$Age, fun = mean)
sapply(imputedTest, function (x) sum(is.na(x)))


# Decision Tree building
dt <- rpart(Survived ~ Pclass + Sex + Age + Fare + SibSp, data=imputedTrain, 
            method="class")
plot(dt)
text(dt)
print(dt)
printcp(dt)
post(dt, 
     filename="~/Desktop/CSCI 183 Data Science/Class Activity 2/titanic_decision_tree")

# Pruned tree
prunedDt<- prune(dt, cp=   dt$cptable[which.min(dt$cptable[,"xerror"]),"CP"])
plot(prunedDt)
text(prunedDt)
printcp(prunedDt)



# Cross validated fitting using gbm
gbmGrid <- expand.grid(interaction.depth = c(1:4), n.trees = seq(200, 10000, 200), 
                       shrinkage = c(.1, .05, .01, .005, .001))
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
set.seed(62461)
gbmFit1 <- train(Survived ~ Pclass + Sex + Age + Fare + SibSp, data=imputedTrain, 
                 method="gbm", trControl=fitControl, verbose=FALSE, tuneGrid = gbmGrid) 


# Predict gbm on the partitioned off test set
pred_test <- round(predict(gbmFit1, newdata = imputedTest))
head(pred_test)
head(test$Survived)
confusionMatrix(data = pred_test, reference = test$Survived)


# Apply model to validation set and predict survival
validate <- read.csv('~/Desktop/CSCI 183 Data Science/Class Activity 2/test.csv')
imputedValidate <- validate
imputedValidate$Age <- impute(validate$Age, fun = mean)
imputedValidate$Fare <- impute(validate$Fare, fun = mean)
sapply(imputedValidate, function (x) sum(is.na(x)))

imputedValidate$Survived <- round(predict(gbmFit1, newdata = imputedValidate))
submission <- subset(imputedValidate, select = c("PassengerId", "Survived"))
write.csv(submission, file="~/Desktop/CSCI 183 Data Science/Class Activity 2/gbm_prediction.csv", row.names=FALSE)
