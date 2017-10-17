
rm(list=ls())
library(dplyr)
library(ggplot2)
setwd('E:\\Sales Analytics\\Data\\Data\\CSVs\\')


df=read.csv('Win loss data for analysis.csv')
names(df)=gsub("\\.","_",names(df))
dim(df)
missing=sapply(df,function(x) sum(is.na(x))+length(which(x=="")))
good_var=names(missing[missing==0])
bad_var=names(missing[missing>0])



#### Merging levels according to the new architecture
# df$Practice=as.character(df$Practice)
# df$Sub.Practice=as.character(df$Sub.Practice)

df[df$Practice=="CXO" & df$Sub_Practice=="EPM","Practice"]="Business Intelligence"

df[df$Practice=="CXO" & df$Sub_Practice=="FRC","Practice"]="Information Management"

df[df$Practice=="MyEngine" & df$Sub_Practice=="Enterprise BI","Practice"]="Business Intelligence"

df[df$Practice=="MyEngine" & df$Sub_Practice=="Enterprise DI","Practice"]="Information Management"

df[df$Practice=="MyViews","Practice"]="Business Intelligence"

df[df$Practice=="MyPlatforms" & df$Sub_Practice=="Enterprise BI","Practice"]="Business Intelligence"

df[df$Status_Reason=="Terminated","Status_Reason"]="Lost"

## Removing the unwanted levels

df$Status_Reason=as.factor(as.character(df$Status_Reason))
df$Practice=as.factor(as.character(df$Practice))


#### Doubts!!!  MyServices   AND   MyWorkplace





### Working with non-missing dataset

df$Miss=apply(df,1,function(x) sum(is.na(x))+length(which(x=="")))
data=df[df$Miss==0,]

#### Modeling....
ind=sample(1:nrow(data),size=floor(70*nrow(data)/100))
train=data[ind,]
test=data[-ind,]

#### Logistic regression
fit=glm(Status_Reason~.,family=binomial(link='logit'),data=train)


