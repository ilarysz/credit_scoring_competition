# -----------------------------------------------------------------------------
# 
#   Author(s):      Pwc 
# 
#   Date     :      05.03.2019 14:41:28 
# 
#   Descr    :      The script presents two ways for Gini index calculation:
#                   - define and use Gini_value() function 
#                     ( execution time ~ 1 sec for 100k observations)
#                   - use rcorr.cens() function from Hmisc package 
#                     ( execution time ~ 50 sec for 100k observations)
# 
# -----------------------------------------------------------------------------



# Input data format

#'> head(test_data)
#'  Application_ID   Score DefFlag
#'1    CCC20000001 0.32452       0
#'2    CCC20000002 0.23452       0
#'3    CCC20000003 0.21421       0
#'4    CCC20000004 0.51253       0
#'5    CCC20000005 0.24125       0
#'6    CCC20000006 0.69321       0


# Define function

Gini_value <- function(
                       score, #prediction from model
                       target #target binary variable (0/1)
                       ){
  
  default <- ifelse(target==0, 'G','B')
  d <- data.frame(FY = default, SCORE = score)
  s <- table(d[,2],d[,1])
  sHeader <- colnames(s)
  s <- cbind(s,apply(s,2,cumsum))
  colnames(s) <- c(sHeader,paste(sHeader,"_cum",sep=""))
  s <- cbind(s , s[,"B_cum"]/max(s[,"B_cum"]) , s[,"G_cum"]/max(s[,"G_cum"]),
             diff(c(0,s[,"G_cum"]))/max(s[,"G_cum"]))
  colnames(s)<-c(sHeader,
                 paste(sHeader,"_cum",sep=""),
                 c("%cum_bad","%cum_good","%good"))
  p <- 1:nrow(s)
  s <- cbind(s, c( s[1,7] , s[p[-1],7]+s[(p-1)[-1],7]) ) 
  s <- cbind(s, c(0,s[1:(nrow(s)-1),"%cum_bad"]))
  colnames(s)[length(colnames(s))] <- "%cum_bad_prev"
  auc <- sum(s[,"%good"]*(s[,"%cum_bad"]+s[,"%cum_bad_prev"])*0.5)
  gini_value <- abs( 2 * ( auc - 0.5 ) )
  return(gini_value)
  
}

# Gini_value(test_data$Score, test_data$DefFlag)


# install.packages('Hmisc')
# library(Hmisc)
# rcorr.cens(test_data$Score, test_data$DefFlag)['Dxy']

