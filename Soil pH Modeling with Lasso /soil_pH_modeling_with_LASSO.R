# Soil pH modeling from remote sensing data using LASSO
# In Class Activity 4
library(glmnet)
library(doMC)
registerDoMC(cores = 6)

data <- read.csv("~/Desktop/CSCI 183 Data Science/Soil pH Modeling with Lasso /soil.csv")

data_mat <- data.matrix(data)

lasso_model <- glmnet(subset(data_mat, select=-pH), subset(data_mat, select=pH), family="gaussian")
plot(lasso_model)

cv_lasso_model <- cv.glmnet(subset(data_mat, select=-pH), subset(data_mat, select=pH), family="gaussian")
coefficients <- as.matrix(coef(cv_lasso_model, cv_lasso_model$lambda.min))
plot(coefficients)


# 10 most significant features
head(coefficients[order(abs(coefficients), decreasing=TRUE),,drop=FALSE], 10)
