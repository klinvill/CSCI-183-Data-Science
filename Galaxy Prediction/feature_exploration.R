# This script contains the code I used to explore implementing edge detection. The ultimate
#   was to try to implement canny-edge detection, however, features extracted using an 
#   autoencoder worked so well for predicting the galaxy data that I abandoned the edge 
#   approach.

# source("load_gp.R")
library("graphics")

lower_bound <<- .1
upper_bound <<- .25

# Grab the most and least smooth galaxies
sample <- train[order(train$Prob_Smooth)[1:10], ]
sample <- rbind(sample, train[order(train$Prob_Smooth, decreasing = TRUE)[1:10], ])

show_edge <- function(row) {
  par(mfrow = c(1, 2)) 
  img <- matrix(as.matrix(subset(as.data.frame(row), select=c(V1:V2500))), nrow=50, ncol=50, byrow=TRUE)
  image(img)
  title(main=paste0("Prob_Smooth: ", row$Prob_Smooth))
  
  edge <- img
  edge[edge < lower_bound] <- 0
  edge[edge > upper_bound] <- 0
  image(edge)
}

edge_to_area <- function(row) {
  img <- matrix(as.matrix(subset(as.data.frame(row), select=c(V1:V2500))), nrow=50, ncol=50, byrow=TRUE)
  edge_length <- length(img[img > lower_bound & img < upper_bound])
  area <- length(img[img > lower_bound])
  return (edge_length/area)
}

display_gals <- function(gals) {
  for (i in 1:nrow(gals)) {
    show_edge(gals[i,])
    print(edge_to_area(gals[i,]))
    Sys.sleep(1)
  }
}

iter_bounds <- function(gal) {
  par(mfrow = c(1, 2)) 
  img <- matrix(as.matrix(subset(as.data.frame(gal), select=c(V1:V2500))), nrow=50, ncol=50, byrow=TRUE)
  for (bound in seq(0, 1, by = 0.01)) {
    edge <- img
    #edge[edge < bound] <- 0
    edge[edge >= bound & edge < bound + .01] <- 1
    image(img)
    image(edge)
    title(main=paste0("Bound: ", bound))
    Sys.sleep(1)
  }
}

iter_smoothed_bounds <- function(gal) {
  par(mfrow = c(1, 2)) 
  img <- matrix(as.matrix(subset(as.data.frame(gal), select=c(V1:V2500))), nrow=50, ncol=50, byrow=TRUE)
  img <- imageData(medianFilter(img, 3))
  dim(img) <- c(50,50)
  for (bound in seq(0, 1, by = 0.01)) {
    edge <- img
    #edge[edge < bound] <- 0
    edge[edge >= bound & edge < bound + .01] <- 1
    image(img)
    image(edge)
    title(main=paste0("Bound: ", bound))
    Sys.sleep(1)
  }
}

iter_bounds(sample[4,])
display_gals(sample)
