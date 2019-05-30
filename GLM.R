# Model optimization

# Data preparation ------
# Setup wd and clear workspace
# rm(list=ls())
# setwd(readClipboard())

# Load necessary libraries
library(Hmisc)
library(caret)
library(e1071)
library(caTools)
library(ggplot2)
library(grid)
library(gridExtra)
library(ROCR)
source("Gini_function.R")
source("stratify.R")

# GLM ------
# Vector of features used during modelling
all_features <- c("Age",	"Job_type",	"Marital_status",	"Home_status",	"Car_status",	"Household_children",	
                  "Monthly_Income",	"Monthly_Spendings",	"Credit_purpose",	"Credit_amount",	"Number_of_installments",
                  "NV_Lag_Mean", "DPD_Lag_Mean", "NO_Lag_Mean", "Free_cash_Credit",
                  "Age_Binned",	"Monthly_Income_Binned",	"Monthly_Spendings_Binned", "Credit_amount_Binned")

used_features <- c("Job_type",	"Marital_status",	"Home_status",	
                   "Credit_purpose",	"Number_of_installments",
                   "NV_Lag_Mean", "DPD_Lag_Mean", "Free_cash_Credit",
                   "Credit_amount_Binned")

# Prepare model formula: paste DefFalg with pasted vector of used features
model_formula <- as.formula(paste("DefFlag~",paste(used_features,collapse="+"),sep=""))

# Model and prediciton
lmodel <- glm(model_formula, data=pwcDataWithFlagScaled, family = "binomial")
summary(lmodel)
prediction_probs <- predict(lmodel, pwcDataWithFlagScaled, type='response')

# Change values to 0 and 1 if in previous step type="prob" was chosen
prediction_1 <- vapply(prediction_probs, function(x) ifelse(x>0.5,1,0), numeric(1))

# Assess model
cm <- table(prediction_1, pwcDataWithFlagScaled[,'DefFlag'])
precision <- cm['1','1'] / (cm['1','1'] + cm['1','0'])
recall <- cm['0','0'] / (cm['0','0'] + cm['0','1'])
accuracy <- (cm['0','0'] + cm['1','1']) / sum(cm)
print(paste("Precision:", precision, "Recall:", recall, "Accuracy:", accuracy, sep=" "))

# Gini score
gini_val <- Gini_value(prediction_probs, pwcDataWithFlag$DefFlag)
print(gini_val)

# Look for optimal cut off points - counting ROC
preds_comp <- prediction(prediction_probs, pwcDataWithFlagScaled[,"DefFlag",drop=T])
perf <- performance(preds_comp,"tpr","fpr")
performance(preds_comp,"auc")
plot(perf)

# This code builds on ROCR library by taking the max delt
# between cumulative bad and good rates being plotted by ROCR
max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])

#function to pick top 3 reasons
#works by sorting coefficient terms in equation
# and selecting top 3 in sort for each loan scored
ftopk<- function(x,top=3){
  res=names(x)[order(x, decreasing = TRUE)][1:top]
  paste(res,collapse=";",sep="")
}
# Produce terms prediciton 
predicition_terms <- predict(lmodel, type='terms', pwcDataWithFlagScaled)
# Application of the function using the top 3 rows
topk=apply(predicition_terms,1,ftopk,top=3)
#add reason list to scored tets sample
test<-cbind(pwcDataWithFlag, topk)

# K-fold cross validation
k = 10
folds <- createFolds(y=pwcDataWithFlagScaled$DefFlag, k=k)
acc_gini <- lapply(folds, function(x){
  train_set <- pwcDataWithFlagScaled[-x,]
  test_set <- pwcDataWithFlagScaled[x,]
  model <- glm(model_formula, data=train_set, family = "binomial")
  prediction_probs <- predict(lmodel, test_set, type='response')
  prediction_1 <- vapply(prediction_probs, function(x) ifelse(x>0.5,1,0), numeric(1))
  cm <- table(prediction_1, test_set[,'DefFlag'])
  accuracy <- (cm['0','0'] + cm['1','1']) / sum(cm)
  gini_val <- Gini_value(prediction_probs, test_set$DefFlag)
  return(c(accuracy, gini_val))
})
print(acc_gini)
