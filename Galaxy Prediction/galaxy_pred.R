# Galaxy Prediction Script
# In Class Activity

setwd("~/Desktop/CSCI 183 Data Science/Galaxy Prediction/")

library(Kmisc)
library(ripa)
library(caret)

source("functions_gp.R")

# source("load_gp.R")
# source("extract_feat_gp.R")
source("train_gp.R")

# Features computed using a denoising autoencoder in Matlab
feats <- read.csv("new_features.csv", header=FALSE)
ids <- read.csv("image_index.csv")
colnames(ids) <- c("id")
feats <- cbind(ids, feats)
sets <- build_sets(feats) 
train_feats <- sets[[1]]
test_feats <- sets[[2]]

set.seed(83902)
cv_index <- createDataPartition(train_feats$Prob_Smooth, times=1, p=.5, list=FALSE)
cv1 <- train_feats[cv_index,]
cv2 <- train_feats[-cv_index,]
cv1$id <- as.numeric(cv1$id)
cv2$id <- as.numeric(cv2$id)

models <- build_models(cv1)
model_errs <- get_best_model(models, cv2)
model <- model_errs[[1]]
errs <- model_errs[[2]]
submit(model, test_feats)
