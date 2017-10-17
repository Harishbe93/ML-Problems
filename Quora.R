rm(list = ls())
setwd("F:\\Mach. Learning\\Kaggle case studies\\Quora Question Pairs")

library(data.table)

dt = fread("train.csv")
test = fread("test.csv")

sapply(dt,function(x) sum(is.na(x)) + length(which(x=="")))