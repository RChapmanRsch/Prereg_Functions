# Look at patterns and freq of missing data  

Missing_Patterns = function(Data){
  Missing_Data_Rows=Data[which(!complete.cases(Data)),]
  if(nrow(Missing_Data_Rows)==0){return("No Missing Data")} else{
  Missing_Data_Rows[!is.na(Missing_Data_Rows)]=" "
  Missing_Data_Rows[is.na(Missing_Data_Rows)]="X"
  return(aggregate(list(Freq=rep(1,nrow(Missing_Data_Rows))), Missing_Data_Rows, length))}}

#for pre-running & Example only:
#Missing_Subset=Data[,c("Age","Gender",PROMIS_Variables$Depression)]
#for(i in 3:6){Missing_Subset[,i]=as.factor(sample(c(NA,1:5), 2000, replace=TRUE))}


# Little's MNAR test & demo/remaining relationship (missing-item/scale)
###will implement this https://web.archive.org/web/20201120030409/https://stats-bayes.com/post/2020/08/14/r-function-for-little-s-test-for-data-missing-completely-at-random/
###source("https://raw.githubusercontent.com/rcst/little-test/master/mcar.R")
#install.packages("norm")
library(norm)
mcar(Missing_Subset[,PROMIS_Variables$Depression])


#for pre-running & Example only - dichotomize missing data:
#Missing_Subset=Data[,c("Age","Gender",PROMIS_Variables$Depression)]
#for(i in 3:5){Missing_Subset[,i]=as.factor(sample(c(1,0), 2000, prob=c(0.1,0.9), replace=TRUE))}


#T-Test and Regression for looking at differences between missing data (dichotomized)
#t.test(Gender~EDDEP04, data=Missing_Subset, var.equal=FALSE)
#summary(lm(Age~EDDEP04, data=Missing_Subset))
