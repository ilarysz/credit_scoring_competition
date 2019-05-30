# Model optimization

# Data preparation ------
# Setup wd and clear workspace
# rm(list=ls())
# setwd(readClipboard())

# Load necessary libraries
library(randomForest)
library(Hmisc)
library(caret)
library(e1071)
library(caTools)
library(ggplot2)
library(lattice)
library(grid)
library(gridExtra)
library(party)
source("Gini_function.R")
source("stratify.R")

# Random Forest part -----
all_features <- c("Age",	"Job_type",	"Marital_status",	"Home_status",	"Car_status",	"Household_children",	
                  "Monthly_Income",	"Monthly_Spendings",	"Credit_purpose",	"Credit_amount",	"Number_of_installments",
                  "NV_Lag_Mean", "DPD_Lag_Mean", "NO_Lag_Mean", "Free_cash_Credit",
                  "Age_Binned",	"Monthly_Income_Binned",	"Monthly_Spendings_Binned", "Credit_amount_Binned")

# used_features <- c("Job_type",	"Marital_status",	"Home_status",	
#                    "Credit_purpose",	"Number_of_installments",
#                    "NV_Lag_Mean", "DPD_Lag_Mean", "Free_cash_Credit",
#                    "Credit_amount_Binned")

used_features <- c("Age",	"Job_type",	"Marital_status",	"Home_status",	"Car_status",	"Household_children",	
                   "Monthly_Income",	"Monthly_Spendings",	"Credit_purpose",	"Credit_amount",	"Number_of_installments")

rfmodel <- randomForest(x=pwcDataWithFlagScaled[,used_features,drop=F], y=pwcDataWithFlagScaled[,"DefFlag",drop=T], ntree=1500)
                        # , mtry = 4)
summary(rfmodel)
rfprediction_probs <- predict(rfmodel, type='prob')

# Change values to 0 and 1 if in previous step type="prob" was chosen
rfprediction_1 <- vapply(rfprediction_probs[,2,drop=T], function(x) ifelse(x>0.5,1,0), numeric(1))

# Assess model
cm <- table(rfprediction_1, pwcDataWithFlagScaled[,'DefFlag'])
precision <- cm['1','1'] / (cm['1','1'] + cm['1','0'])
recall <- cm['0','0'] / (cm['0','0'] + cm['0','1'])
accuracy <- (cm['0','0'] + cm['1','1']) / sum(cm)
print(paste("Precision:", precision, "Recall:", recall, "Accuracy:", accuracy, sep=" "))

# Gini score
gini_val <- Gini_value(rfprediction_probs, pwcDataWithFlag$DefFlag)
print(gini_val)

# K-fold cross validation
k = 10
folds <- createFolds(y=pwcDataWithFlagScaled$DefFlag, k=k)
acc_gini <- lapply(folds, function(x){
  train_set <- pwcDataWithFlagScaled[-x,]
  test_set <- pwcDataWithFlagScaled[x,]
  model <- randomForest(x=train_set[,used_features,drop=F], y=train_set[,'DefFlag',drop=T], ntree=300,
                        importance = T, keep.forest = T, mtry = length(used_features) - 2)
  prediction_probs <- predict(model, newdata = test_set[,used_features,drop=F], type='prob')
  prediction_1 <- vapply(prediction_probs[,2,drop=T], function(x) ifelse(x>0.5,1,0), numeric(1))
  cm <- table(test_set[,'DefFlag'], prediction_1)
  accuracy <- (cm['0','0'] + cm['1','1']) / sum(cm)
  gini_val <- Gini_value(prediction_probs[,2,drop=T], test_set$DefFlag)
  return(c(accuracy, gini_val))
})
print(acc_gini)

# Grid search
classifier = train(form = model_formula, data = pwcDataWithFlagScaled[1:1000,], method = "cforest", na.action = na.omit)

metric <- "Accuracy"
control <- trainControl(method="repeatedcv", number=10, repeats=1, search="grid")
tunegrid <- expand.grid(.mtry=c(1:length(used_features)))
pwcDataStrat <- stratified(pwcDataWithFlagScaled, group="DefFlag", size=0.1)
rf_gridsearch <- train(form = model_formula, 
                       data = pwcDataWithFlagScaled[1:10000,], 
                       method="rf", 
                       metric=metric, 
                       tuneGrid=tunegrid, 
                       trControl=control,
                       na.action = na.omit)
print(rf_gridsearch)
plot(rf_gridsearch)

# Variables choice
# Test variables
test_features <- c("Age",	"Job_type",	"Marital_status",	"Home_status",	"Car_status",	"Household_children",	
                  "Monthly_Income",	"Monthly_Spendings",	"Credit_purpose",	"Credit_amount",	"Number_of_installments",
                  "NV_Lag_Mean", "DPD_Lag_Mean", "NO_Lag_Mean")

# Create combinations
all_combs <- list()
for(i in 10:length(test_features)){
  all_combs[[i]] <- combn(test_features, i, simplify = T)
}

# List will carry result arrays for given lengths
results_i <- list()
# Observation are in the list with elements names of length, i is length to be considered in each iteration
start <- Sys.time()
for(i in 10:length(test_features)){
  # Array will carry results for the combination of given length
  ar <- array(rep(NA,i+4), dim = c(dim(all_combs[[i]])[2],i+4))
  # Iterate over all combination of given length
  for(j in 1:dim(all_combs[[i]])[2]){
    # Take set of features to train model
    test_features <- all_combs[[i]][,j]
    
    # Model and prediciton
    rfmodel <- randomForest(x=pwcDataWithFlag[,test_features,drop=F], y=pwcDataWithFlag[,"DefFlag",drop=T], ntree=20)
    rfprediction_probs <- predict(rfmodel, type='prob')
    
    # Change values to 0 and 1 if in previous step type="prob" was chosen
    rfprediction_1 <- vapply(rfprediction_probs[,2,drop=T], function(x) ifelse(x>0.5,1,0), numeric(1))
    
    # Assess model
    cm <- table(rfprediction_1, pwcDataWithFlagScaled[,'DefFlag'])
    # Ifs take into account probability that model did not predict any 1's and cm is 1-D
    if(sum(dim(cm)) == 3){
      precision <- 0
      recall <- 0
      accuracy <- 0
      gini_val <- 0
    }else if(sum(dim(cm)) == 4){
      precision <- cm['1','1'] / (cm['1','1'] + cm['1','0'])
      recall <- cm['0','0'] / (cm['0','0'] + cm['0','1'])
      accuracy <- (cm['0','0'] + cm['1','1']) / sum(cm)
      # Gini score
      gini_val <- Gini_value(rfprediction_probs, pwcDataWithFlag$DefFlag)
    }else{
      precision <- 0
      recall <- 0
      accuracy <- 0
      gini_val <- 0
    }

    # Append value the array of results for the given combination
    ar[j,] <- c(test_features, precision, recall, accuracy, gini_val)
  }
  # Store array of results for given length at the list (element name is length)
  results_i[[i]] <- ar
}
end <- Sys.time()
# Find best results in each element of the list
list_max <- list()
for(i in 10:length(results_i)){
  # Select the max record basing on the last column in given array (gini)
  list_max[[i]] <- results_i[[i]][which.max(results_i[[i]][,dim(results_i[[i]])[2]]),]
}


