# Trains a regression model for the Galaxy Prediction script

library(randomForest)
library(gbm)
library(caret)
library(doMC)
registerDoMC(cores=6)

build_models <- function(train) {
  # Builds a variety of models
  #
  # Args:
  #   train: dataset to train models on
  
  glm_1 <- glm(Prob_Smooth ~ .-id, data=train)
  
  set.seed(276)
  rf_1 <- randomForest(x=subset(train, select=-c(id, Prob_Smooth)), y=train$Prob_Smooth)
  
  set.seed(829345)
  gbm_1 <- gbm.fit(x=subset(train, select=-c(id, Prob_Smooth)), y=train$Prob_Smooth, 
                   distribution = "gaussian")
  
  train_control <- trainControl(method="cv")
  
  set.seed(276)
  rf_2 <- train(x=subset(train, select=-c(id, Prob_Smooth)), y=train$Prob_Smooth, 
                   method="rf", trControl=train_control)
  
  set.seed(829345)
  gbm_2 <- train(x=subset(train, select=-c(id, Prob_Smooth)), y=train$Prob_Smooth, 
                   method="gbm", trControl=train_control)
  
  models <- list(glm_1, rf_1, gbm_1, rf_2, gbm_2)
  return (models)
}

get_best_model <- function(models, validate) {
  # Returns the best model in terms of RMSE along with the RMSE of each model
  #
  # Args:
  #   models: list of models to evaluate
  #   validate: validation data to test the models agains
  
  error_rates <- sapply(models, function(model) {
    pred <- predict(model, validate)
    rmse <- postResample(pred, validate$Prob_Smooth)[[1]]
    return (rmse)
  })
  
  return (list(models[[which.min(error_rates)]], error_rates))
}





