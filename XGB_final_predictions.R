library(xgboost)
library(caret)
source("Gini_function.R")
source("stratify.R")

# Predictions for all observations 
# Read the data
pwcData <- read.table("final_data.csv",header=TRUE, sep=",")
features_names <- colnames(pwcData)
char_features_names <- c("Job_type",	"Marital_status",	"Home_status",	"Car_status", "Credit_purpose")

# Convert Notional value and due columns 
cols_to_process <- grep("NotionalValue_lag\\d+$", colnames(pwcData))
x = pwcData[,cols_to_process]
x = rowSums(x) / length(cols_to_process)
pwcData["NV_Lag_Mean"] = x

cols_to_process <- grep("DPD_lag\\d+$", colnames(pwcData))
x = pwcData[,cols_to_process]
x = rowSums(x) / length(cols_to_process)
pwcData["DPD_Lag_Mean"] = x

cols_to_process <- grep("NotionalOverdue_lag\\d+$", colnames(pwcData))
x = pwcData[,cols_to_process]
x = rowSums(x) / length(cols_to_process)
pwcData["NO_Lag_Mean"] = x

# One hot encoding
dmy <- dummyVars(" ~ .", data = pwcData[char_features_names])
trsf <- data.frame(predict(dmy, newdata = pwcData))
trsf_fin <- cbind(trsf, pwcData[, !(names(pwcData) %in% char_features_names)])

# Conversion to matrix that can be handled by the algo
traind <- mapply(trsf_fin, FUN=as.numeric)

# Removing redundant variables 
traind <- traind[,-which(colnames(traind) == "Application_ID" | colnames(traind) == "GEO_region")]
# Remove one option per each dummy variable
no_unique_per_dummy <- sapply(char_features_names, function(x) length(attr(pwcData[1,x], "levels")))
dummies_to_drop <- c()
for(i in 1:length(no_unique_per_dummy)){
  dummies_to_drop <- c(dummies_to_drop, sum(no_unique_per_dummy[1:i]))
}
traind <- traind[,-dummies_to_drop]
# Additional removal options that can be considered:
# Remove DPD, Notonial value and Notional overdue columns
traind <- traind[,-(which(colnames(traind) == "NotionalValue_t0"):which(colnames(traind) == "NotionalOverdue_lag12"))]

DefFlag_location <- which(colnames(traind) == 'DefFlag')

# Train model on each observation that is flagged
xgb_model <- xgboost(data = traind[1:70000,-DefFlag_location],  # Data
                     label = traind[1:70000,DefFlag_location,drop=T],
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

# Make predicitons
xgb_preds <- predict(xgb_model, traind[,-DefFlag_location])
print(Gini_value(xgb_preds[1:70000],traind[1:70000,DefFlag_location]))

outputData = data.frame(Application_ID = pwcData$Application_ID,Score = xgb_preds)
write.csv(outputData,"CaseStudy.csv")






