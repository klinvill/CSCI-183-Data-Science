# Extracts and constructs relevant features for the Galaxy Prediction Script

start_time_extract <- proc.time()
train_feats <- extract(train) 
test_feats <- extract(test)
stop_time_extract <- proc.time()
stop_time_extract-start_time_extract




