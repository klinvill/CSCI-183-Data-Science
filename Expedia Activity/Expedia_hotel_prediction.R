# Expedia Competition for CSCI 183
#
# Goal: predict whether or not a user will purchase a hotel stay
#
# Variable Meanings:
#     srch_id -- search ID
#     date_time -- date-time of search
#     site_id -- Expedia Point Of Sale ID
#     visitor_location_country_id -- country ID of customer
#     visitor_hist_starrating -- mean star rating of hotels previously purchased by 
#                                customer, null (0) signifies no purchase history
#     visitor_hist_adr_usd -- mean price per night in US$ of the hotels the customer 
#                             has previously purchased, null (0) signifies no purchase 
#                             history
#     prop_country_id -- country ID of hotel location
#     prop_id -- hotel ID
#     prop_starrating -- star rating of hotel in seq(1,5,1), 0 signifies an unknown 
#                        rating or a rating that cannot be publicized
#     prop_review_score -- mean customer review score for the hotel on a scale out of 5
#                          rounder to .5 increments. 0 means no reviews, null that the 
#                          info is not available
#     prop_brand_bool -- 1 if the hotel is part of a major chain, 0 if it is independent
#     prop_location_score1 -- First score indicating the desirability of the hotel's 
#                             location
#     prop_location_score2 -- Second score outlining the desirability of the hotel's 
#                             location
#     prop_log_historical_price -- The logarithm of the mean price of the hotel over the
#                                  last trading period. 0 will occur if the hotel was not
#                                  sold in that period
#     price_usd -- Displayed price of the hotel for the given search. Note that different
#                  countries have different conventions regarding displaying taxes and 
#                  fees and the value may be per night or for the whole stay
#     promotion_flag -- 1 if the hotel had a sale price promotion displayed
#     srch_destination_id -- ID of the destination where the hotel search was performed
#     srch_length_of_stay -- number of nights stay that was searched
#     srch_booking_window -- number of days in the future the hotel stay started from 
#                            the search date
#     srch_adults_count -- number of adults specified in the room
#     srch_children_count -- number of (extra occupancy)children specified in the hotel 
#                            room
#     srch_room_count -- number of hotel rooms specified in the search
#     srch_saturday_night_bool -- 1 if the stay includes a Saturday night, starts from 
#                                 Thursday with a length of stay is less than or equal
#                                 to 4 nights (i.e. weekend), otherwise 0
#     srch_query_affinity_score -- log of the probability a hotel will be clicked on in
#                                  Internet searches. null (0) signifies no data.
#     orig_destination_distance -- Physical distance between the hotel and the customer
#                                  at the time of the search. null (0) means it could not be
#                                  calculated.
#     random_bool -- 1 when the displayed sort was random, 0 when the normal sort order
#                    was displayed
#     comp1_rate -- 1 if Expedia has a lower price than competitor 1 for the hotel, 0 if
#                   the same, -1 if Expedia's price is higher. null signifies no data
#     comp1_inv -- 1 if the competitor doesn't have availability in the hotel. 0 if 
#                  both Expedia and the competitor have availability. null signifies 
#                  no data
#     comp1_rate_percent_diff -- Absolute percentage difference between Expedia and the
#                                competitor's price. null signifies no data
#     comp2_* -- same as above but for competitor 2 
#     ...
#     comp8_* -- same as above but for competitor 8 
#     booking_bool -- 1 indicates the customer purchased a hotel stay, otherwise 0
#
# Observations: 
#     Most customers have no previous data (preliminary)
#     visitor_hist_adr_usd is a better indicator of whether a customer is new or not
#         since visitor_hist_starrating could be 0 due to the previous hotel not having 
#         a star rating

library(caret)
library(rpart)
library(lubridate)
library(arm)
library(ROCR)
library(randomForest)
library(doMC)
registerDoMC(cores = 6)


################################################################
######                                                    ######
######       FUNCTIONS USED THROUGHOUT THE SCRIPT         ######
######                                                    ######
################################################################

# The data is transformed within a function to make it easy to apply the same transformations
#   to the validation set
transform_data <- function(data){
  data$prop_starrating <- factor(data$prop_starrating)
  data$prop_review_score <- factor(data$prop_review_score)
  
  data$date_time <- as.POSIXct(data$date_time, format = "%m/%d/%y %H:%M")
  data$month <- factor(month(data$date_time))
  data$wday <- factor(wday(data$date_time))
  
  # Returning customers will have a non-zero value for visitor_hist_adr_usd while new
  #     customers will have a zero value
  data$returning_customer <- as.numeric(data$visitor_hist_adr_usd != 0)
  
  # Treating searches as an observation
  search_features <- c("srch_id", "date_time", "site_id", "visitor_location_country_id", 
                       "visitor_hist_starrating", "visitor_hist_adr_usd", "srch_destination_id", 
                       "srch_length_of_stay", "srch_booking_window", "srch_adults_count", 
                       "srch_children_count", "srch_room_count", "srch_saturday_night_bool", 
                       "random_bool", "returning_customer")
  # booking_bool should only be a feature for the training and test sets, not the validation set
  if ("booking_bool" %in% colnames(data)) 
    search_features <- c(search_features, "booking_bool")
  
  search_info <- subset(data, select=search_features)
  
  unique_srch_ids <- unique(data$srch_id)
  
  # booking_bool should only be a feature for the training and test sets, not the validation set
  ifelse ("booking_bool" %in% colnames(data),
          search_data <- aggregate(booking_bool~., search_info, function(x) x[which.max(abs(x))]),
          search_data <- unique(search_info))
  
  search_data$wday <- wday(search_data$date_time)
  search_data$month <- month(search_data$date_time)
  
  return (list(as.data.frame(data), as.data.frame(search_data)))
}


# Test function returns AUC
test_model <- function(model, test_data, method) {
  ifelse(method %in% c("gbm"), 
         pred_test <- round(predict(model, newdata = test_data)),
         ifelse(method %in% c("glm", "randomForest"), 
                pred_test <- round(predict(model, newdata = test_data, type="response")),
                stop("Only gbm, glm, and randomForest methods are supported")))
         
  pred <- prediction(pred_test, test_data$booking_bool)
  perf <- as.numeric(slot(performance(pred, measure = "auc"), "y.values"))
  return (perf)
}


# Returns both the glm model and AUC measure
# Takes as input the names of the columns to be used in the right side of the formula for
#   training and the data to use for training and testing
run_and_test_glm <- function(important_fields, train_data, test_data){
  important_formula <- as.formula(paste("booking_bool ~ ", paste(important_fields, 
                                                                 collapse="+")))
  
  # build model
  set.seed(62461)
  model <- glm(important_formula, data=train_data, family=binomial("logit"))
  
  # measure auc
  perf <- test_model(model, test_data, "glm")
  
  return(list(model, perf))
}


# Returns both the gbm model and AUC measure
# Takes as input the names of the columns to be used in the right side of the formula for
#   training and the data to use for training and testing
run_and_test_gbm <- function(important_fields, train_data, test_data, trControl = trainControl(), 
                             tuneGrid=NULL, verbose=FALSE){
  important_formula <- as.formula(paste("booking_bool ~ ", paste(important_fields, 
                                                                 collapse="+")))
  
  # build model
  set.seed(62461)
  model <- train(important_formula, data=train_data, 
                 method="gbm", trControl=trControl, verbose=verbose, tuneGrid = tuneGrid)
  
  # measure auc
  perf <- test_model(model, test_data, "gbm")
  
  return(list(model, perf))
}




################################################################
######                                                    ######
######           TRAINING AND TEST DATA LOADING           ######
######                                                    ######
################################################################

data <- read.csv('~/Desktop/CSCI 183 Data Science/Expedia Activity/train.csv')
head(data)

t_data <- transform_data(data)
data <- t_data[[1]]
search_data <- t_data[[2]]
remove(t_data)

set.seed(6291)
srchTrainIndex <- createDataPartition(search_data$booking_bool, times = 1, p = .8, list=FALSE)
srchTrain <- search_data[srchTrainIndex,]
srchTest <- search_data[-srchTrainIndex,]

train <- data[data$srch_id %in% srchTrain$srch_id,]
test <- data[data$srch_id %in% srchTest$srch_id,]

# remove unneeded variables to save space
rm(srchTrainIndex)
rm(data)
rm(search_data)


train_sum <- summary(train)

# Check to see if any variables need to be imputed
sum(sapply(train, function(x) sum(is.na(x))))
sum(sapply(test, function(x) sum(is.na(x))))
sum(sapply(train, function(x) sum(is.null(x))))
sum(sapply(test, function(x) sum(is.null(x))))










################################################################
######                                                    ######
######          EXPLORATORY DATA ANALYSIS SECION          ######
######                                                    ######
################################################################

# Each srch_id has at most 1 booking, roughly half of all searches result in a booking
# One potential approach could be to predict whether or not the search will result in a 
#   booking, and then choose the most likely entry with that given srch id to be booked
qplot(booking_bool, data=subset(train, srch_id==1), binwidth=1)
nrow(subset(train, booking_bool==1))
nrow(unique(subset(train, booking_bool==1, select=srch_id)))

# returning customers seem to have slightly higher booking rates
ggplot(train) + stat_bin(aes(x=returning_customer, y=..density..), 
                         binwidth=0.5) + facet_grid(.~booking_bool)

# locations don't seem to strongly correlate with booking
featurePlot(x = train[,c("site_id", "visitor_location_country_id", 
                         "prop_country_id", "booking_bool")], 
            y=factor(train$booking_bool), plot = "ellipse", auto.key=list(columns=2))

# Hypothesis: there is likely a relationship between customer rating and star rating
# Verdict: the mean customer rating seems to linearly model star rating. In particular,
#           higher star hotels have fewer low customer ratings.
star_v_cust <- sapply(c(1:5), function(x) subset(train, prop_starrating==x & 
                                                   prop_review_score != 0, 
                                                 select=prop_review_score))
sapply(star_v_cust, summary)
sapply(star_v_cust, median)
sapply(star_v_cust, sd)
plot(c(1:5), sapply(star_v_cust, mean))
sapply(star_v_cust, length)
# very few reviews come from hotels with 1-star ratings
qplot(c(1:5), sapply(star_v_cust, length), geom="bar", stat="identity")

# It seems less accurate to deduce customer rating based off of stars
cust_v_star <- sapply(c(1:5), function(x) subset(train, prop_starrating!=0 & 
                                                   prop_review_score == x, 
                                                 select=prop_starrating))
sapply(cust_v_star, summary)
sapply(cust_v_star, median)
sapply(cust_v_star, sd)
plot(c(1:5), sapply(cust_v_star, mean))
sapply(cust_v_star, length)
qplot(c(1:5), sapply(cust_v_star, length), geom="bar", stat="identity")

# star and customer ratings seems to matter little for booking except 4 star hotels 
# have a slightly higher booking rate while 2 star hotels have a slightly lower booking 
# rate
ggplot(data = train) + stat_density(aes(x=prop_starrating, y=..scaled.., 
                                        color=factor(booking_bool)), 
                                    position="identity", geom="line")
ggplot(data = train) + stat_density(aes(x=prop_review_score, y=..scaled.., 
                                        color=factor(booking_bool)), 
                                    position="identity", geom="line")

# Major chain hotels have a just slightly larger chance of having a customer book
sum(subset(train, prop_brand_bool==0)$booking_bool==1)/sum(train$prop_brand_bool==0)
sum(subset(train, prop_brand_bool==1)$booking_bool==1)/sum(train$prop_brand_bool==1)

# prop_location_score1 and prop_log_historical_price seem to have little effect on the 
# outcome while prop_location_score2 may have a slight impact 
ggplot(data=train) + stat_density(aes(x=prop_location_score1, y=..density.., 
                                      color=factor(booking_bool)), 
                                  position="identity", fill=NA)
ggplot(data=train) + stat_density(aes(x=prop_location_score2, y=..density.., 
                                      color=factor(booking_bool)), 
                                  position="identity", fill=NA)
ggplot(data=train) + stat_density(aes(x=prop_log_historical_price, y=..density.., 
                                      color=factor(booking_bool)), 
                                  position="identity", fill=NA)

# lower search prices are much more likely to be booked, generally around $100
featurePlot(x = train[,c("prop_log_historical_price", "price_usd", "promotion_flag")], 
            y=factor(train$booking_bool), plot = "pairs", auto.key=list(columns=2))
qplot(price_usd, data=train, geom="density")
qplot(price_usd, data=subset(train, price_usd < .4), geom="density", 
      facets=.~booking_bool)

# long stays and distant booking windows seem to not be purchased often
featurePlot(x = train[,c("srch_destination_id", "srch_length_of_stay", 
                         "srch_booking_window")], 
            y=factor(train$booking_bool), plot = "pairs", auto.key=list(columns=2))
# neither adult count, child count, or room count seem to be significant
featurePlot(x = train[,c("srch_adults_count", "srch_children_count", 
                         "srch_room_count")], 
            y=factor(train$booking_bool), plot = "ellipse", auto.key=list(columns=2))
ggplot(data = train) + stat_density(aes(x=srch_room_count, y=..scaled.., 
                                        color=factor(booking_bool)), 
                                    position="identity", geom="line")

# srch_saturday_night_bool seems to have a little influence over booking
ggplot(train) + stat_bin(aes(x=srch_saturday_night_bool, y=..density..), 
                         binwidth=0.5) + facet_grid(.~booking_bool)

# affinity score seems to have little effect on booking
featurePlot(x=train$srch_query_affinity_score, y=factor(train$booking_bool), plot = "box")
ggplot(data = train) + stat_bin(aes(x=srch_query_affinity_score, y=..count.., 
                                        color=factor(booking_bool)), 
                                    position="identity", fill=NA)
ggplot(data = subset(train, srch_query_affinity_score<0)) + stat_bin(aes(x=srch_query_affinity_score, y=..density.., 
                                    color=factor(booking_bool)), 
                                position="identity", fill=NA)

# orig_destination_distance seems to have little effect
qplot(orig_destination_distance, data=train, color=factor(booking_bool), geom="density")

# random displayed order seems to strongly negatively correlate to booking
ggplot(train) + stat_bin(aes(x=random_bool, y=..density..), 
                         binwidth=0.5) + facet_grid(.~booking_bool)

# expedia seems to always have better or about the same rates as competitor 1
featurePlot(x = train[,c("comp1_rate", "comp1_inv", 
                         "comp1_rate_percent_diff")], 
            y=factor(train$booking_bool), plot = "ellipse", auto.key=list(columns=2))
featurePlot(x = train[,c("comp2_rate", "comp2_inv", 
                         "comp2_rate_percent_diff")], 
            y=factor(train$booking_bool), plot = "ellipse", auto.key=list(columns=2))


# Date doesn't appear to affect booking
featurePlot(x=train[,c("month", "wday", "year", "booking_bool")], y=factor(train$booking_bool), 
            plot="ellipse", auto.key=list(columns=2))





# Search approach

# random bool is a very strong indication of whether or not a search will result in a 
#   booking
featurePlot(subset(search_data, select = c(srch_saturday_night_bool, random_bool)), 
            factor(search_data$booking_bool), plot = "density", auto.key=list(columns=2))
# srch_length_of_stay and srch_booking_window are weak indicators of booking
featurePlot(subset(search_data, select = c(srch_length_of_stay)), 
            factor(search_data$booking_bool), plot = "density", auto.key=list(columns=2))
featurePlot(subset(search_data, select = c(srch_booking_window)),
            factor(search_data$booking_bool), plot = "density", auto.key=list(columns=2))
# srch_adults_count and srch_saturday_night_bool are very weak indicators
featurePlot(subset(search_data, select = c(srch_adults_count)),
            factor(search_data$booking_bool), plot = "density", auto.key=list(columns=2))
featurePlot(subset(search_data, select = c(srch_saturday_night_bool)),
            factor(search_data$booking_bool), plot = "density", auto.key=list(columns=2))


# The booked rooms tend to be in the lower half of the price options and seem to be less than ~$250
ggplot(data[data$srch_id %in% unique_srch_ids[1:20],], aes(srch_id, price_usd)) +
  geom_point(aes(shape=factor(booking_bool), color=factor(booking_bool), size=1), 
             position=position_dodge(width=0.75))
ggplot(data[data$srch_id %in% unique_srch_ids[21:40],], aes(srch_id, price_usd)) +
  geom_point(aes(shape=factor(booking_bool), color=factor(booking_bool), size=1), 
             position=position_dodge(width=0.75))
ggplot(data[data$srch_id %in% unique_srch_ids[41:60],], aes(srch_id, price_usd)) +
  geom_point(aes(shape=factor(booking_bool), color=factor(booking_bool), size=1), 
             position=position_dodge(width=0.75))
ggplot(subset(data, booking_bool==1), aes(price_usd, ..count../sum(..count..))) + geom_density()
ggplot(subset(data, booking_bool==1 & price_usd<5000), aes(price_usd, ..count../sum(..count..))) + 
  geom_density()
ggplot(subset(data, booking_bool==1 & price_usd<1000), aes(price_usd, ..count../sum(..count..))) + 
  geom_density()


# srch_query_affinity_score seems to correlate to bookings, but very few searches have this score
ggplot(data[data$srch_id %in% unique_srch_ids[1:20],], aes(srch_id, srch_query_affinity_score)) +
  geom_point(aes(shape=factor(booking_bool), color=factor(booking_bool), size=1), 
             position=position_dodge(width=0.75))
ggplot(data[data$srch_id %in% unique_srch_ids[21:40],], aes(srch_id, srch_query_affinity_score)) +
  geom_point(aes(shape=factor(booking_bool), color=factor(booking_bool), size=1), 
             position=position_dodge(width=0.75))
ggplot(data[data$srch_id %in% unique_srch_ids[41:60],], aes(srch_id, srch_query_affinity_score)) +
  geom_point(aes(shape=factor(booking_bool), color=factor(booking_bool), size=1), 
             position=position_dodge(width=0.75))
ggplot(subset(data, booking_bool==1 & srch_query_affinity_score), aes(srch_query_affinity_score, ..count../sum(..count..))) + 
  geom_density()
ggplot(subset(data, booking_bool==0 & srch_query_affinity_score), aes(srch_query_affinity_score, ..count../sum(..count..))) + 
  geom_density()


# rating doesn't seem to matter
featurePlot(subset(data, select = c(prop_starrating, prop_review_score)),
            factor(data$booking_bool), plot = "density", auto.key=list(columns=2))
ggplot(subset(data, booking_bool==0), aes(prop_starrating, ..count../sum(..count..))) + 
  geom_density()
ggplot() + geom_histogram(aes(x=prop_starrating, y=..count../sum(..count..), 
                              color=factor(booking_bool)), data=subset(data, booking_bool==0),
                          binwidth=1, fill=NA) + 
  geom_histogram(aes(x=prop_starrating, y=..count../sum(..count..), color=factor(booking_bool)), 
                 data=subset(data, booking_bool==1), binwidth=1, fill=NA)


featurePlot(subset(data, select = c(prop_location_score2)),
            factor(data$booking_bool), plot = "density", auto.key=list(columns=2))



################################################################
######                                                    ######
######               MODEL TRAINING SECION                ######
######                                                    ######
################################################################

all_fields <- colnames(train)
unimportant_fields <- c("srch_id", "date_time", "booking_bool")
important_fields <- setdiff(all_fields, unimportant_fields)
important_formula <- as.formula(paste("booking_bool ~ ", paste(important_fields, collapse="+")))


# Basic decision tree attempt, no useful results
dt <- rpart(important_formula, data=train, method="class")
print(dt)
printcp(dt)
prunedDt<- prune(dt, cp=   dt$cptable[which.min(dt$cptable[,"xerror"]),"CP"])
print(prunedDt)
printcp(prunedDt)


# Basic boosting attempt
gbmGrid <- expand.grid(interaction.depth = c(1:4), n.trees = seq(200, 10000, 200), 
                       shrinkage = c(.1, .05, .01, .005, .001))
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
set.seed(62461)
gbmFit1 <- train(important_formula, data=train, 
                 method="gbm", trControl=fitControl, verbose=FALSE) 
set.seed(62461)
gbmFit2 <- train(important_formula, data=train, 
                 method="gbm", trControl=fitControl, verbose=FALSE, tuneGrid = gbmGrid) 


# Predict gbm on the partitioned off test set
pred_test <- round(predict(gbmFit1, newdata = test))
head(pred_test)
head(test$booking_bool)
confusionMatrix(data = pred_test, reference = test$booking_bool)


# Logistic Regression Attempt
temp <- run_and_test_glm(important_fields, train, test)
glmFit1 <- temp[[1]]
glmPerf1 <- temp[[2]]
summary(glmFit1)

# adjusted important_fields based off of significance of each factor in glmFit1
important_fields <- c("prop_starrating", "prop_review_score", "prop_brand_bool", 
                      "prop_country_id", "prop_location_score1", "prop_location_score2",
                      "prop_log_historical_price", "price_usd", "promotion_flag", 
                      "srch_length_of_stay", "srch_booking_window", "srch_adults_count",
                      "srch_children_count", "srch_room_count", "srch_query_affinity_score",
                      "random_bool", "comp1_rate", "comp1_inv", "comp1_rate_percent_diff",
                      "comp2_rate", "comp2_inv", "comp2_rate_percent_diff", 
                      "comp3_rate", "comp3_inv", "comp3_rate_percent_diff", 
                      "comp4_rate", "comp4_inv", "comp4_rate_percent_diff", 
                      "comp5_rate", "comp5_inv", "comp5_rate_percent_diff", 
                      "comp6_rate", "comp6_inv", "comp6_rate_percent_diff",
                      "comp7_rate", "comp7_inv", "comp7_rate_percent_diff",
                      "comp8_rate", "comp8_inv", "comp8_rate_percent_diff", 
                      "returning_customer")
temp <- run_and_test_glm(important_fields, train, test)
glmFit2 <- temp[[1]]
glmPerf2 <- temp[[2]]
summary(glmFit2)

test$pred <- round(predict(glmFit2, newdata = test, type="response"))
confusionMatrix(data = test$pred, reference = test$booking_bool)


# glmFit2 is still not predicting many bookings, try narrowing features more
important_fields <- c("prop_starrating", "prop_review_score", "prop_brand_bool", 
                      "prop_country_id", "prop_location_score1",
                      "prop_log_historical_price", "price_usd", "promotion_flag", 
                      "srch_length_of_stay", "srch_booking_window", "srch_adults_count",
                      "srch_children_count", "srch_room_count", 
                      "random_bool", "returning_customer")
temp <- run_and_test_glm(important_fields, train, test)
glmFit3 <- temp[[1]]
glmPerf3 <- temp[[2]]
summary(glmFit3)

test$pred <- round(predict(glmFit3, newdata = test, type="response"))
head(test$pred)
head(test$booking_bool)
confusionMatrix(data = test$pred, reference = test$booking_bool)


# glmFit3 still doesn't predict any bookings, EDA may give us a better choice of vars
important_fields <- c("prop_brand_bool", "prop_location_score2", "price_usd", 
                      "srch_length_of_stay", "srch_booking_window", "random_bool")
temp <- run_and_test_glm(important_fields, train, test)
glmFit4 <- temp[[1]]
glmPerf4 <- temp[[2]]
summary(glmFit4)

test$pred <- round(predict(glmFit4, newdata = test, type="response"))
head(test$pred)
head(test$booking_bool)
confusionMatrix(data = test$pred, reference = test$booking_bool)

# glmFit4 still doesn't predict any bookings. Imputing zero values for some variables, 
#   and centering and scaling variables may help
train$prop_starrating[train$prop_starrating==0] <- NA
train$prop_review_score[train$prop_review_score==0] <- NA
test$prop_starrating[test$prop_starrating==0] <- NA
test$prop_review_score[test$prop_review_score==0] <- NA
pre_proc <- preProcess(train[,!colnames(train) %in% c("date_time", "date", "booking_bool")], method=c("center", "scale", "knnImpute"))
temp <- predict(pre_proc, train[,!colnames(train) %in% c("date_time", "date", "booking_bool")] )
test$pred <- NULL
temp2 <- predict(pre_proc, test[,!colnames(test) %in% c("date_time", "date", "booking_bool")] )

write.csv(temp, file="~/Desktop/CSCI 183 Data Science/Expedia\ Activity/temp1.csv")
write.csv(temp2, file="~/Desktop/CSCI 183 Data Science/Expedia\ Activity/temp2.csv")

# Center and scale variables for a better regression fit
#sapply(train, class)
#pre_proc <- preProcess(train[,!colnames(train) %in% c("date_time", "date", "booking_bool")], method=c("center", "scale"))
#train[,!colnames(train) %in% c("date_time", "date", "booking_bool")] <- predict(pre_proc, train[,!colnames(train) %in% c("date_time", "date", "booking_bool")])
#test[,!colnames(test) %in% c("date_time", "date", "booking_bool")] <- predict(pre_proc, test[,!colnames(test) %in% c("date_time", "date", "booking_bool")])

# Load imputed values into train and test 
train[,!colnames(train) %in% c("date_time", "date", "booking_bool")] <- temp
test[,!colnames(test) %in% c("date_time", "date", "booking_bool")] <- temp2




important_fields <- c("prop_starrating", "prop_review_score", "prop_brand_bool", 
                      "prop_country_id", "prop_location_score1",
                      "prop_log_historical_price", "price_usd", "promotion_flag", 
                      "srch_length_of_stay", "srch_booking_window", "srch_adults_count",
                      "srch_children_count", "srch_room_count", 
                      "random_bool", 
                      "comp1_rate", "comp1_inv", "comp1_rate_percent_diff",
                      "comp2_rate", "comp2_inv", "comp2_rate_percent_diff", 
                      "comp3_rate", "comp3_inv", "comp3_rate_percent_diff", 
                      "comp4_rate", "comp4_inv", "comp4_rate_percent_diff", 
                      "comp5_rate", "comp5_inv", "comp5_rate_percent_diff", 
                      "comp6_rate", "comp6_inv", "comp6_rate_percent_diff",
                      "comp7_rate", "comp7_inv", "comp7_rate_percent_diff",
                      "comp8_rate", "comp8_inv", "comp8_rate_percent_diff",
                      "returning_customer")
temp <- run_and_test_glm(important_fields, train, test)
glmFit5 <- temp[[1]]
glmPerf5 <- temp[[2]]
summary(glmFit5)

test$pred <- round(predict(glmFit5, newdata = test, type="response"))
head(test$pred)
head(test$booking_bool)
confusionMatrix(data = test$pred, reference = test$booking_bool)


important_fields <- c("prop_brand_bool", "prop_location_score2", "price_usd", 
                      "srch_length_of_stay", "srch_booking_window", "random_bool",
                      "comp1_rate", "comp1_inv", "comp1_rate_percent_diff",
                      "comp2_rate", "comp2_inv", "comp2_rate_percent_diff", 
                      "comp3_rate", "comp3_inv", "comp3_rate_percent_diff", 
                      "comp4_rate", "comp4_inv", "comp4_rate_percent_diff", 
                      "comp5_rate", "comp5_inv", "comp5_rate_percent_diff", 
                      "comp6_rate", "comp6_inv", "comp6_rate_percent_diff",
                      "comp7_rate", "comp7_inv", "comp7_rate_percent_diff",
                      "comp8_rate", "comp8_inv", "comp8_rate_percent_diff")
temp <- run_and_test_glm(important_fields, train, test)
glmFit6 <- temp[[1]]
glmPerf6 <- temp[[2]]
summary(glmFit6)

test$pred <- round(predict(glmFit6, newdata = test, type="response"))
head(test$pred)
head(test$booking_bool)
confusionMatrix(data = test$pred, reference = test$booking_bool)


important_fields <- c("prop_starrating", "prop_review_score", "prop_brand_bool", 
                      "prop_country_id", "prop_location_score1", "prop_location_score2",
                      "prop_log_historical_price", "price_usd", "promotion_flag", 
                      "srch_length_of_stay", "srch_booking_window", "srch_adults_count",
                      "srch_children_count", "srch_room_count", "srch_query_affinity_score",
                      "random_bool", "returning_customer")
temp <- run_and_test_glm(important_fields, train, test)
glmFit7 <- temp[[1]]
glmPerf7 <- temp[[2]]
summary(glmFit7)

test$pred <- round(predict(glmFit7, newdata = test, type="response"))
head(test$pred)
head(test$booking_bool)
confusionMatrix(data = test$pred, reference = test$booking_bool)


important_fields <- c("prop_brand_bool", "prop_location_score2", "price_usd", 
                      "srch_length_of_stay", "srch_booking_window", "random_bool",
                      "comp1_rate", "comp1_inv", "comp1_rate_percent_diff",
                      "comp2_rate", "comp2_inv", "comp2_rate_percent_diff", 
                      "comp3_rate", "comp3_inv", "comp3_rate_percent_diff", 
                      "comp4_rate", "comp4_inv", "comp4_rate_percent_diff", 
                      "comp5_rate", "comp5_inv", "comp5_rate_percent_diff", 
                      "comp6_rate", "comp6_inv", "comp6_rate_percent_diff",
                      "comp7_rate", "comp7_inv", "comp7_rate_percent_diff",
                      "comp8_rate", "comp8_inv", "comp8_rate_percent_diff",
                      "returning_customer")
temp <- run_and_test_glm(important_fields, train, test)
glmFit8 <- temp[[1]]
glmPerf8 <- temp[[2]]
summary(glmFit8)

test$pred <- round(predict(glmFit8, newdata = test, type="response"))
head(test$pred)
head(test$booking_bool)
confusionMatrix(data = test$pred, reference = test$booking_bool)


important_fields <- c("prop_brand_bool", "prop_location_score2", "price_usd", 
                      "srch_length_of_stay", "srch_booking_window", "random_bool",
                      "comp1_rate", "comp2_rate", "comp3_rate", "comp4_rate", 
                      "comp5_rate", "comp6_rate", "comp7_rate", "comp8_rate", 
                      "returning_customer")
temp <- run_and_test_glm(important_fields, train, test)
glmFit9 <- temp[[1]]
glmPerf9 <- temp[[2]]
summary(glmFit9)

test$pred <- round(predict(glmFit9, newdata = test, type="response"))
head(test$pred)
head(test$booking_bool)
confusionMatrix(data = test$pred, reference = test$booking_bool)






# Attempt to train using searches
important_fields <- c("srch_id", "date_time", "site_id", 
                      "visitor_location_country_id", 
                      "visitor_hist_starrating", "visitor_hist_adr_usd", 
                      "srch_destination_id", "srch_length_of_stay", 
                      "srch_booking_window", "srch_adults_count", 
                      "srch_children_count", "srch_room_count", 
                      "srch_saturday_night_bool", "random_bool")
important_formula <- as.formula(paste("booking_bool ~ ", paste(important_fields, collapse="+")))

sdt <- rpart(important_formula, data=srchTrain, method="class")
print(sdt)
printcp(sdt)
prunedSDt<- prune(sdt, cp=   sdt$cptable[which.min(sdt$cptable[,"xerror"]),"CP"])
print(prunedSDt)
printcp(prunedSDt)

gbmGrid <- expand.grid(interaction.depth = c(1:4), n.trees = seq(200, 10000, 200), 
                       shrinkage = c(.1, .05, .01, .005, .001))
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
set.seed(62461)
sgbmFit1 <- train(important_formula, data=srchTrain, 
                 method="gbm", trControl=fitControl, verbose=FALSE) 
# Predict gbm on the partitioned off test set
pred_test <- round(predict(sgbmFit1, newdata = srchTest))
head(pred_test)
head(srchTest$booking_bool)
confusionMatrix(data = pred_test, reference = srchTest$booking_bool)
sgbmPred1 <- prediction(pred_test, srchTest$booking_bool)
sgbmPerf1 <- as.numeric(slot(performance(sgbmPred1, measure = "auc"), "y.values"))

set.seed(62461)
sgbmFit2 <- train(important_formula, data=srchTrain, 
                 method="gbm", trControl=fitControl, verbose=FALSE, tuneGrid = gbmGrid)
# Predict gbm on the partitioned off test set
pred_test <- round(predict(sgbmFit2, newdata = srchTest))
head(pred_test)
head(srchTest$booking_bool)
confusionMatrix(data = pred_test, reference = srchTest$booking_bool)
sgbmPred2 <- prediction(pred_test, srchTest$booking_bool)
sgbmPerf2 <- as.numeric(slot(performance(sgbmPred2, measure = "auc"), "y.values"))














# Function to make predictions based off a 2-stage prediction
# In this assignment, the first stage uses only search data, the second stage is carried out
#   within each positively predicted stage
predict_split_model <- function(data, search_data, stage1_predictor, stage2_predictor){

  search_data$spred <- round(predict(stage1_predictor, newdata=search_data))
  
  end_predictions <- sapply(search_data$srch_id, function(x){ 
                              ifelse(search_data[search_data$srch_id==x,]$spred==0, 
                                     data[data$srch_id==x,]$pred <- 0,
                                     data[data$srch_id==x,]$pred <- round(predict(stage2_predictor, 
                                                                          newdata=data[data$srch_id==x,])))
                              return (data$pred)
                            })
  
  # TODO: Only keep the most likely prediction for each search
  
  return (end_predictions)
}




all_fields <- colnames(train)
unimportant_fields <- c("srch_id", "date_time", "booking_bool")
important_fields <- setdiff(all_fields, unimportant_fields)
important_formula <- as.formula(paste("booking_bool ~ ", paste(important_fields, collapse="+")))


set.seed(62461)
rfFit1 <- randomForest(x=subset(train, srch_id %in% as.list(subset(srchTrain, booking_bool==1)$srch_id), select=-c(booking_bool)), 
                       y=subset(train, srch_id %in% as.list(subset(srchTrain, booking_bool==1)$srch_id))$booking_bool, 
                       ntree=10, do.trace=TRUE)
rfFit2 <- randomForest(x=train[,colnames(train) %in% important_fields], y=train$booking_bool, 
                       ntree=100, do.trace=TRUE)
rfFit3 <- randomForest(x=train[,colnames(train) %in% important_fields], y=train$booking_bool, 
                       ntree=30, do.trace=TRUE)





# The best predictor so far has been glmFit8 with centered and scaled variables
bestPredictor <- glmFit8
bestPredictionRate <- gPerf8


################################################################
######                                                    ######
######             FINAL PREDICTION SECION                ######
######                                                    ######
################################################################

# Predict on validation set
validate <- read.csv('~/Desktop/CSCI 183 Data Science/Expedia Activity/test.csv')
temp3 <- validate[, c("srch_id", "prop_id")]

# Check to see if any variables need to be imputed
sum(sapply(validate, function(x) sum(is.na(x))))
sum(sapply(validate, function(x) sum(is.null(x))))

# Apply the same transformations used on the train and test sets to the validate set
t_validate <- transform_data(validate) 
validate <- t_validate[[1]]
val_search_data <- t_validate[[2]]
remove(t_validate)


val_search_data$spred <- round(predict(sgbmFit1, newdata=val_search_data, type="prob"))


#validate[,!colnames(validate) %in% c("date_time")] <- predict(pre_proc, validate[,!colnames(validate) %in% c("date_time")])
validate$pred <- round(predict(rfFit3, newdata=validate, type="response"))
head(validate)
submission <- data.frame(paste(validate$srch_id, validate$prop_id, sep="-"), validate$pred) 
names(submission)<- c("srch-prop_id", "booking_bool")
head(submission)

write.csv(submission, "~/Desktop/CSCI 183 Data Science/Expedia Activity/pred2.csv", row.names=FALSE)
