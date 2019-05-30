# Model optimization

# Data preparation ------
# Setup wd and clear workspace
rm(list=ls())
setwd(readClipboard())

# Load necessary libraries
library(ggplot2)
library(lattice)
library(grid)
library(gridExtra)


# Load data, choose only flagged data 
pwcData = read.table("final_data.csv",header=TRUE, sep=",")
pwcDataWithFlag = pwcData[1:70000,]

# convert specified columns to factors
pwcDataWithFlag[,'DefFlag'] <- factor(pwcDataWithFlag[,'DefFlag'], levels = c(0,1), labels=c(0,1))
pwcDataWithFlag[,"Number_of_installments"] <- factor(pwcDataWithFlag[,'Number_of_installments'], 
                                                     levels = unique(pwcDataWithFlag[,'Number_of_installments']), 
                                                     labels = unique(pwcDataWithFlag[,'Number_of_installments']))
pwcDataWithFlag[,"Household_children"] <- factor(pwcDataWithFlag[,"Household_children"],
                                                 levels = unique(pwcDataWithFlag[,"Household_children"]),
                                                 labels = unique(pwcDataWithFlag[,"Household_children"]))

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

# Scale features
to_scale <- c("Age",	"Monthly_Income",	"Monthly_Spendings", "Credit_amount")
pwcDataWithFlagScaled <- pwcDataWithFlag
pwcDataWithFlagScaled[, to_scale] <- scale(pwcDataWithFlagScaled[, to_scale])
to_scale <- c("NV_Lag_Mean", "DPD_Lag_Mean", "NO_Lag_Mean")
pwcDataWithFlagScaled[, to_scale] <- scale(pwcDataWithFlagScaled[, to_scale])

# Calculate ratio of income - spendings to credit amount
pwcDataWithFlagScaled$Free_cash_Credit <- ((pwcDataWithFlag$Monthly_Income - pwcDataWithFlag$Monthly_Spendings) /
                                             pwcDataWithFlag$Credit_amount)

to_scale <- c("Age",	"Monthly_Income",	"Monthly_Spendings", "Credit_amount")
# Create bins basing on quartiles and then factorize
for(stat in to_scale){
  qs <- quantile(pwcDataWithFlag[,stat,drop=T])
  pwcDataWithFlagScaled[,paste(stat,"_Binned",sep="")] <- as.factor(ifelse(pwcDataWithFlag[,stat]<qs[2],qs[2],
                                                                           ifelse(pwcDataWithFlag[,stat]<qs[3],qs[3],
                                                                                  ifelse(pwcDataWithFlag[,stat]<qs[4],qs[4],qs[5]))))
}


# EDA --------

worth_considering <- c("Age",	"Job_type",	"Marital_status",	"Home_status",	"Car_status",	"Household_children",	
                       "Monthly_Income",	"Monthly_Spendings",	"Credit_purpose",	"Credit_amount",	"Number_of_installments",
                       "NV_Lag_Mean", "DPD_Lag_Mean", "NO_Lag_Mean")

numeric_variables <- c("Age",	
                       "Monthly_Income",	"Monthly_Spendings",	"Credit_amount",
                       "NV_Lag_Mean", "DPD_Lag_Mean", "NO_Lag_Mean")

categorical_variables <- worth_considering[!(worth_considering %in% numeric_variables)]

# Check basic features of data
summary(pwcDataWithFlag[worth_considering])
head(pwcDataWithFlag[worth_considering])
tail(pwcDataWithFlag[worth_considering])
str(pwcDataWithFlag[worth_considering])

# Check correlation of data
# corrs = as.matrix(cor(pwcDataWithFlag[c(-1,-4,-5,-6,-7,-11)]))
corrs = as.matrix(cor(pwcDataWithFlag[numeric_variables]))
# Useful if taking into account more variables 
corrs_high <- which(corrs > 0.85, arr.ind=T)
corrs_names <- data.frame(rownames(corrs_high),
                          colnames(corrs)[corrs_high[, "col"]],
                          corrs[corrs > 0.85])

# Generate histograms for each variable
for(i in categorical_variables){
  print(ggplot(pwcDataWithFlag, aes(x=pwcDataWithFlag[,i,drop=T], fill=pwcDataWithFlag[,"DefFlag",drop=T])) +
          geom_histogram(stat='count') +
          labs(x=i, y="counts", title=paste("Counts of", i, sep = " "), fill="Default"))
}

for(i in numeric_variables){
  print(ggplot(pwcDataWithFlag, aes(x=pwcDataWithFlag[,i,drop=T], fill=pwcDataWithFlag[,"DefFlag",drop=T])) +
          geom_histogram(bins=sqrt(length(unique(pwcDataWithFlag[,i,drop=T])))) +
          labs(x=i, y="counts", title=paste("Counts of", i, sep = " "), fill="Default"))
}

grid.newpage()
pushViewport(viewport(layout = grid.layout(ncol = 4, nrow = 1)))
# pushViewport(viewport(x=0.0, width=0.4, just="left"))
i = 1
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
pushViewport(plotViewport(margins = rep(0.5,4)))
print(bwplot(x=pwcDataWithFlag[,numeric_variables[i]], xlab=numeric_variables[i]), newpage=F)
popViewport(2)
i = 2
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
pushViewport(plotViewport(margins = rep(0.5,4)))
print(bwplot(x=pwcDataWithFlag[,numeric_variables[i]], xlab=numeric_variables[i]), newpage=F)
popViewport(2)
i = 4
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 3))
pushViewport(plotViewport(margins = rep(0.5,4)))
print(bwplot(x=pwcDataWithFlag[,numeric_variables[i]], xlab=numeric_variables[i]), newpage=F)
popViewport(2)
i = 6
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 4))
pushViewport(plotViewport(margins = rep(0.5,4)))
print(bwplot(x=pwcDataWithFlag[,numeric_variables[i]], xlab=numeric_variables[i]), newpage=F)
popViewport(2)


