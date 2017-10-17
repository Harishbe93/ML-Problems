setwd("F:\\Mach. Learning\\Kaggle case studies\\Movies Recommendation")
library(data.table)
movies = fread("u.data",sep = "\t")
names(movies) = c("")