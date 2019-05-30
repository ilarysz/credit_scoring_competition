library(xgboost)
library(caret)
source("Gini_function.R")
source("stratify.R")

# Data preparation for XGB has separate procedures from the previous models
pwcData <- read.table("final_data.csv",header=TRUE, sep=",")
pwcDataWithFlag <- pwcData[1:70000,]

# Convert Notional value and due columns 
cols_to_process <- grep("NotionalValue_lag\\d+$", colnames(pwcDataWithFlag))
x = pwcDataWithFlag[,cols_to_process]
x = rowSums(x) / length(cols_to_process)
pwcDataWithFlag["NV_Lag_Mean"] = x

cols_to_process <- grep("DPD_lag\\d+$", colnames(pwcDataWithFlag))
x = pwcDataWithFlag[,cols_to_process]
x = rowSums(x) / length(cols_to_process)
pwcDataWithFlag["DPD_Lag_Mean"] = x

cols_to_process <- grep("NotionalOverdue_lag\\d+$", colnames(pwcDataWithFlag))
x = pwcDataWithFlag[,cols_to_process]
x = rowSums(x) / length(cols_to_process)
pwcDataWithFlag["NO_Lag_Mean"] = x

xgb_modelling <- function(features_names, char_features_names){
  # One hot encoding
  dmy <- dummyVars(" ~ .", data = pwcDataWithFlag[char_features_names])
  trsf <- data.frame(predict(dmy, newdata = pwcDataWithFlag))
  trsf_fin <- cbind(trsf, pwcDataWithFlag[, !(names(pwcDataWithFlag) %in% char_features_names)])
  
  # Prepare the vector of colnames to take into consideration during modelling 
  cols_for_xgb <- c(colnames(trsf), features_names[!(features_names %in% char_features_names)])
  
  # Conversion to matrix that can be handled by the algo
  traind <- mapply(trsf_fin[c(cols_for_xgb, "DefFlag")], FUN=as.numeric)

  # Removing redundant variables 
  traind <- traind[,-which(colnames(traind) == "Application_ID" | colnames(traind) == "GEO_region")]
  # Remove one option per each dummy variable
  no_unique_per_dummy <- sapply(char_features_names, function(x) length(attr(pwcDataWithFlag[1,x], "levels")))
  dummies_to_drop <- c()
  for(i in 1:length(no_unique_per_dummy)){
    dummies_to_drop <- c(dummies_to_drop, sum(no_unique_per_dummy[1:i]))
  }
  traind <- traind[,-dummies_to_drop]
  # Additional removal options that can be considered:
  # Remove DPD, Notonial value and Notional overdue columns
  traind <- traind[,-(which(colnames(traind) == "NotionalValue_t0"):which(colnames(traind) == "NotionalOverdue_lag12"))]
  # Turn off the following line to not exclude any features basing  on their importance
  # traind <- traind[,c(as.vector(top_gain_vars[['Feature']]), "DefFlag", "DefFlag.1")]

  # Create train and test sets with constant ratio of bad/good clients
  stratified_df <- stratified(pwcDataWithFlag, group="DefFlag", size=0.7)
  train_set <- traind[(rownames(pwcDataWithFlag) %in% rownames(stratified_df)),,drop=F]
  test_set <- traind[!(rownames(pwcDataWithFlag) %in% rownames(stratified_df)),,drop=F]
  
  # Convert DefFlag column to integer with 0 and 1 values
  train_set[,'DefFlag'] <- as.integer(train_set[,'DefFlag',drop=T])
  test_set[,'DefFlag'] <- as.integer(test_set[,'DefFlag',drop=T])
  
  # Remove one redundant DefFlag column
  DefFlag_location <- which(colnames(train_set) == 'DefFlag.1')
  train_set <- train_set[,-DefFlag_location]
  test_set <- test_set[,-DefFlag_location]
  
  # Save the DefFlag column location
  DefFlag_location <- which(colnames(train_set) == 'DefFlag')
  
  # Default settings
  xgb_model <- xgboost(data = train_set[,-DefFlag_location],
                       label = train_set[,DefFlag_location,drop=T],
                       nrounds = 100,
                       verbose = 1,
                       nthread = 8)
  
  cat("Gini for the default settings: ", "\n")
  
  xgb_preds <- predict(xgb_model, train_set[,-DefFlag_location])
  cat("Gini on train set", Gini_value(xgb_preds,train_set[,DefFlag_location]), "\n")
  
  xgb_preds <- predict(xgb_model, test_set[,-DefFlag_location])
  cat("Gini on test set", Gini_value(xgb_preds,test_set[,DefFlag_location]), "\n")
  
  # Tuned settings
  xgb_model <- xgboost(data = train_set[,-DefFlag_location],  # Data
                       label = train_set[,DefFlag_location,drop=T],
                       max_depth = 3, 
                       subsample = 0.8,
                       gamma = 1,
                       nrounds = 1000,
                       colsample_bytree = 0.5,
                       min_child_weight = 1,
                       eta = 0.1,
                       objective = "binary:logistic", # Constants
                       eval_metric = "logloss",
                       verbose = 1, # Constants - control
                       nthread = 8)
  
  cat("Gini for tuned settings: ", "\n")
  
  xgb_preds <- predict(xgb_model, train_set[,-DefFlag_location])
  cat("Gini on train set", Gini_value(xgb_preds,train_set[,DefFlag_location]), "\n")
  
  xgb_preds <- predict(xgb_model, test_set[,-DefFlag_location])
  cat("Gini on test set", Gini_value(xgb_preds,test_set[,DefFlag_location]), "\n")
  
  # Assesment of importance metrics
  xgb.plot.importance(xgb.importance(model=xgb_model))
  top_gain_vars <<- as.data.frame(xgb.importance(model=xgb_model)[1:10,"Feature"])
}

# Modelling and variable choice
xgb_modelling(colnames(pwcDataWithFlag), 
              c("Job_type",	"Marital_status",	"Home_status",	"Car_status", "Credit_purpose"))

top_gain_vars <- list('Feature' = c("NO_Lag_Mean","DPD_Lag_Mean","Monthly_Income","Job_type.Full.time","Employed_finance_number",
                   "Home_status.Owner","Marital_status.Divorced","Marital_status.Maried","Credit_amount","Age"))


# Tuning parameters
# set up the cross-validated hyper-parameter search
# "Traind" data set can be created by using functions within "xgb_modelling" function
xgb_grid_1 = expand.grid(
  nrounds = 100,
  max_depth = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20),
  eta = seq(0.0, 0.3, length.out = 7),
  gamma = c(1, 10, 50, 100),
  colsample_bytree = 1, 
  min_child_weight = 1, 
  subsample = c(0.5, 0.6, 0.7, 0.8, 0.9, 1)
)

# Look for optimal hyper-params
for(i in 1:nrow(xgb_grid_1)){
  print(paste("Iteration No. ", i))
  xgb_model <- xgboost(data = traind[,-DefFlag_location],  # Data
                       label = traind[,DefFlag_location,drop=T],
                       max_depth = xgb_grid_1[i,"max_depth",drop=T], # Controlled by grid
                       subsample = xgb_grid_1[i,"subsample",drop=T],
                       gamma = xgb_grid_1[i,"gamma",drop=T],
                       nrounds = xgb_grid_1[i,"nrounds",drop=T],
                       colsample_bytree = xgb_grid_1[i,"colsample_bytree",drop=T],
                       min_child_weight = xgb_grid_1[i,"min_child_weight",drop=T],
                       eta = xgb_grid_1[i,"eta",drop=T],
                       objective = "binary:logistic", # Constants
                       eval_metric = "auc",
                       verbose = 0, # Constants - control
                       nthread = 8)
  
  xgb_preds <- predict(xgb_model, train_set[,-DefFlag_location])
  xgb_grid_1[i,"Gini_train"] <- Gini_value(xgb_preds,train_set[,DefFlag_location])
  
  xgb_preds <- predict(xgb_model, test_set[,-DefFlag_location])
  xgb_grid_1[i,"Gini_test"] <- Gini_value(xgb_preds,test_set[,DefFlag_location])
}

# Show parameters with best Gini for test set
xgb_grid_1[which.max(xgb_grid_1[,"Gini_test"]),]

# Grid search with "stock" function
# Basing on the grid defined earlier
# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 1,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all", # save losses across all models
  classProbs = TRUE,    # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = FALSE
)

# train the model for each parameter combination in the grid,
#   using CV to evaluate
DefFlag_location <- which(colnames(train_set) == 'DefFlag')
xgb_train_1 = train(
  x = traind[,-DefFlag_location,drop=F],
  objective = "binary:logistic",
  eval_metric = "auc",
  y = as.factor(traind[,DefFlag_location,drop=T]),
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  metric = "Accuracy",
  method = "xgbDART"
)

# scatter plot of the AUC against max_depth and eta
ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) +
  geom_point() +
  theme_bw() +
  scale_size_continuous(guide = "none")


