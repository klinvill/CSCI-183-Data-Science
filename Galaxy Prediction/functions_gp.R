# Functions used in the Galaxy Prediction scripts

process_image <- function(img_path) {
  img <- readJPEG(img_path)
  grey_img <- rgb2grey(img)
  grey_img <- ripa::normalize(grey_img)
  # grab the center half of the image and reduce it down to 50x50
  grey_img <- resize(grey_img[106:317, 106:317], 50, 50)
  
  # gets the image id from the image file name
  img_id <- sub("^([^.]*).*", "\\1", basename(img_path))
  
  # flatten image matrix to be easily stored in a dataframe
  dim(grey_img) <- NULL
  temp <- as.data.frame(rbind(NULL, grey_img), row.names = c(as.character(img_id)))
  temp$id <- img_id
  temp <- subset(temp, select=c(id, V1:V2500))
  return (temp)
}

build_image_set <- function(img_dir) {
  imgs = list.files(img_dir, full.names=TRUE)
  img_chunks = split(imgs, ceiling(seq_along(imgs)/1000))
  
  images <- do.call(rbind, 
                    pbsapply(img_chunks, function(chunk) {
                      t(sapply(chunk, process_image))
                    },
                    simplify=FALSE))
  
  
  return (as.data.frame(images))
}

build_sets <- function (images) {
  train_results <- read.csv("galaxy_train.csv")
  
  # split images into train and test sets
  train <- subset(images, id %in% train_results$GalaxyID)
  test <- subset(images, !(id %in% train_results$GalaxyID))
  
  train <- merge(train, train_results, by.x="id", by.y="GalaxyID")
  
  return (list(train, test))
}

extract <- function (images) {
  if ("Prob_Smooth" %in% colnames(images)) {
    img_features <- subset(images, select=c(id, Prob_Smooth))
    imgs <- subset(images, select=-c(id, Prob_Smooth))
  } else {
    img_features <- subset(images, select=c(id))
    imgs <- subset(images, select=-c(id))
  }
  img_features$mean <- apply(imgs, 1, mean)
  img_features$var <- apply(imgs, 1, var)
  quantiles <- apply(imgs, 1, quantile, probs=as.numeric(c(.1, .25, .75, .9)))
  img_features <- cbind(img_features, t(quantiles))
  return(img_features)
}

submit <- function(model, test_features) {
  # Creates a submission file to submit to Kaggle
  # 
  # Args: 
  #   model: model to use to make the predictions on the test set
  #   test_features: the test set
  
  submission <- NULL
  submission$GalaxyID <- test_features$id
  test_features$id <- as.numeric(test_features$id)
  submission$Prob_Smooth <- predict(model, test_features)
  submission <- as.data.frame(submission)
  write.csv(submission, "submission.csv", row.names=FALSE)
}