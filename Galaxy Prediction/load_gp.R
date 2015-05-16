# Loads the Data for the Galaxy Prediction Script
# This script essentially loads all the images, shrinks them down to 50x50, 
#     and associates them with their id and training/test sets

setwd("~/Desktop/CSCI 183 Data Science/Galaxy Prediction/")

library(ripa)
library(jpeg)
library(EBImage)
library(pbapply)

start_time_load <- proc.time()
images <- build_image_set("images_training_rev1/")
# Transforms a list of lists into a data frame
images <- as.data.frame(lapply(images, function(X) unname(unlist(X))))
sets <- build_sets(images)
train <- sets[[1]]
test <- sets[[2]]
rm(sets)
stop_time_load <- proc.time()
