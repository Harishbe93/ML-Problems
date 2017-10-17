### Setting up workspace...
rm(list=ls())
setwd("F:\\Mach. Learning\\Kaggle case studies\\Zillow")

### Loadina all libraries...
library(data.table)
library(caret)
library(ggplot2)
library(dplyr)


### Read the input files...

prop = fread("properties_2016.csv")
trans = fread("train_2016.csv")

### Analyzing the transactions file...

trans[,transactiondate := as.Date(transactiondate,"%Y-%m-%d")]
trans[,month := format(transactiondate,"%m")]

###

trans %>% group_by(month) %>% summarize(Count = n()) %>% ggplot(aes(x=month,y=Count))+geom_bar(stat='identity')

prop %>% 