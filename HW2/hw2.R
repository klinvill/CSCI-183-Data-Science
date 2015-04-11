# Goal:
#     Predict survival or death for each passenger
#
# General Observations:
#     Class mattered, more 1st class people had a >50% chance of survival, 2nd class was
#         roughly 50-50, 3rd class had ~25% chance of survival
#     Sex mattered, a large majority of females survived while a large majority of males 
#         died
#     Having 1 or two siblings/spouses and/or parents/children aboard increased odds of 
#         survival, but mainly only for females
#     Very young children were likely to survive while young men were very likely to die

library(ggplot2)
library(Amelia)
library(Hmisc)

train <- read.csv("~/Desktop/CSCI 183 Data Science/HW2/train.csv")
test <- read.csv("~/Desktop/CSCI 183 Data Science/HW2/test.csv")

# Exploratory Data Analysis
qplot(factor(Survived), data=train, facets=.~Pclass)
qplot(factor(Survived), data=train, facets=.~Sex)
qplot(factor(SibSp), data=train, facets=Sex~Survived)
qplot(factor(Parch), data=train, facets=Sex~Survived)
qplot(Age, data=train, facets=Sex~Survived, binwidth=5)
qplot(Fare, data=train, facets=.~Pclass, geom="density")
train$CabinGroup <- substring(train$Cabin, 1, 1)
train$CabinNum <- substring(train$Cabin, 2)
qplot(CabinGroup, data=train, facets=.~Pclass)
qplot(CabinNum, data=subset(train, CabinNum != ""), facets=Survived~.)
qplot(Fare, data=subset(train, CabinGroup != ""), facets=CabinGroup~.)

missmap(train, main="Titanic Training Data - Missings Map", col=c("yellow", "black"), legend=FALSE)

# Extract titles from the names
train$Title = rapply(strsplit(as.character(train$Name), "[,.] "), function(name) name[2])
summary(factor(train$Title))
qplot(factor(train$Title))

bystats(train$Age, train$Title, fun=median)

# Impute Age by using the median age per each title
get_medians <- function(data) {
  titles <- unique(data$Title)
  medians <- sapply(titles, function(title) median(subset(train, Title==title & !is.na(Age))$Age))
  medians <- impute(medians, median)
  medians
}

impute_median_by_title <- function(data, medians) {
  for (i in 1:nrow(data)){
    if (is.na(data[i,]$Age)){
      data[i,]$Age <- medians[[data[i,]$Title]]
    }
  }
  # Return the data frame
  data
}

train <- impute_median_by_title(train, get_medians(train))

# Logistic Regression with class, sex, and age
# all variables significant
classifier_1 <- glm(Survived ~ Pclass + Sex + Age, data=train, family="binomial")
summary(classifier_1)

# Logistic Regression with siblings/spouses and parents/children in addition to class, sex, and age
# parents/children insignificant
classifier_2 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch, data=train, family="binomial")
summary(classifier_2)

# Logistic Regression with Embarked in addition to siblings/spouses class, sex, and age
# Embarked insignificant
classifier_3 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Embarked, data=train, family="binomial")
summary(classifier_3)

# The best classifier of the bunch
classifier_4 <- glm(Survived ~ Pclass + Sex + Age + SibSp, data=train, family="binomial")
summary(classifier_4)
confint(classifier_4)

# Same as classifier 4 but treats class as a factor. Actually gives a worse response.
classifier_5 <- glm(Survived ~ factor(Pclass) + Sex + Age + SibSp, data=train, family="binomial")
summary(classifier_5)
confint(classifier_5)

classifier_6 <- glm(Survived ~ Pclass + Sex, data=train, family="binomial")
summary(classifier_6)

classifier_7 <- glm(Survived ~ Pclass + Sex + SibSp, data=train, family="binomial")
summary(classifier_7)

classifier_8 <- glm(Survived ~ factor(Pclass) + Sex + SibSp, data=train, family="binomial")
summary(classifier_8)

# Check predictions against training set
head((predict(classifier_4, type="response") > 0.5) + 0)
head(train$Survived)

# Impute the test data
test$Title = rapply(strsplit(as.character(test$Name), "[,.] "), function(name) name[2])
missmap(test, main="Titanic Training Data - Missings Map", col=c("yellow", "black"), legend=FALSE)
bystats(test$Age, test$Title, fun=median)
# Use the training and test set medians
test <- impute_median_by_title(test, get_medians(rbind(subset(train, select=c("Age", "Title")), subset(test, select=c("Age", "Title")))))
missmap(test, main="Titanic Training Data - Missings Map", col=c("yellow", "black"), legend=FALSE)
bystats(test$Age, test$Title, fun=median)

# Predict test survival using classifier_4
test$Survived <- ((predict(classifier_4, newdata=test, type="response") > 0.5) + 0)
submission <- subset(test, select=c("PassengerId", "Survived"))
head(submission)

write.csv(submission, file="~/Desktop/CSCI 183 Data Science/HW2/prediction_4.csv", row.names=FALSE)


# Predict test survival using classifier_1
test$Survived <- ((predict(classifier_1, newdata=test, type="response") > 0.5) + 0)
submission <- subset(test, select=c("PassengerId", "Survived"))
head(submission)

write.csv(submission, file="~/Desktop/CSCI 183 Data Science/HW2/prediction_1.csv", row.names=FALSE)


# Predict test survival using classifier_6
test$Survived <- ((predict(classifier_6, newdata=test, type="response") > 0.5) + 0)
submission <- subset(test, select=c("PassengerId", "Survived"))
head(submission)

write.csv(submission, file="~/Desktop/CSCI 183 Data Science/HW2/prediction_6.csv", row.names=FALSE)


# Predict test survival using classifier_7, best result on kaggle so far likely do to my imputing the Ages
#   first classifier to beat the gender model
test$Survived <- ((predict(classifier_7, newdata=test, type="response") > 0.5) + 0)
submission <- subset(test, select=c("PassengerId", "Survived"))
head(submission)

write.csv(submission, file="~/Desktop/CSCI 183 Data Science/HW2/prediction_7.csv", row.names=FALSE)


# Predict test survival using classifier_8
test$Survived <- ((predict(classifier_8, newdata=test, type="response") > 0.5) + 0)
submission <- subset(test, select=c("PassengerId", "Survived"))
head(submission)

write.csv(submission, file="~/Desktop/CSCI 183 Data Science/HW2/prediction_8.csv", row.names=FALSE)
