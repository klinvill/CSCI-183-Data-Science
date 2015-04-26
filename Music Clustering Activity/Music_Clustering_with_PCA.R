# Music Clustering with Principal Component Analysis 
# In Class Activity 3

library(Hmisc)

data <- read.csv("~/Desktop/CSCI 183 Data Science/Music Clustering Activity/music-all.csv")

# TODO: impute based on median per artist instead of overall median
#artist_med <- aggregate(subset(data, select=-c("X", "type")), , median)

data[, !colnames(data) %in% c("X", "artist", "type")] <- impute(data[, !colnames(data) %in% c("X", "artist", "type")]) 

pca <- prcomp(data[, !colnames(data) %in% c("X", "artist", "type")], retx=TRUE, scale=TRUE)

# plotted standard deviations of the principle components
qplot(pca$sdev, geom="density")

p2 <- data.frame(as.data.frame(pca$x)$PC1, as.data.frame(pca$x)$PC2, data$artist)
names(p2) <- c("first_PC", "second_PC", "artist")

ggplot(p2, aes(x=first_PC, y=second_PC, label=artist)) + geom_point() + geom_text()
