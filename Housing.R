rm(list = ls())
setwd("F:\\Mach. Learning\\Kaggle case studies\\Housing Prediction")

train = read.csv("train.csv",stringsAsFactors=F)
test = read.csv("test.csv",stringsAsFactors=F)

### Combine the datasets to do descriptive analysis and missing value handling...
df = rbind(within(train,rm("Id","SalePrice")),within(test,rm("Id")))

### Finding out the columns containing missing values...
na.cols = which(colSums(is.na(df))>0)
sort(sapply(df[,names(na.cols)],function(x) sum(is.na(x))),decreasing = T)

### A helper function for plotting categorical data...
plot.categoric = function(cols, df){
  for (col in cols) {
    order.cols = names(sort(table(df[,col]), decreasing = TRUE))
  
    num.plot = qplot(df[,col]) +
      geom_bar(fill = 'cornflowerblue') +
      geom_text(aes(label = ..count..), stat='count', vjust=-0.5) +
      theme_minimal() +
      scale_y_continuous(limits = c(0,max(table(df[,col]))*1.1)) +
      scale_x_discrete(limits = order.cols) +
      xlab(col) +
      theme(axis.text.x = element_text(angle = 30, size=12))
  
    print(num.plot)
  }
}

### Only 10 values are present in PoolQC...
plot.categoric("PoolQC",df)

### Check fr PoolArea bcoz if PoolArea is 0 then we can assume that there is no pool at all...
### There are only three records for which PoolArea is not zero and PoolQC is missing...
df[df$PoolArea>0 & is.na(df$PoolQC),c("PoolArea","PoolQC")]

### Fill the values with the nearest match
library(data.table)
dt=data.table(df)

dt[,.(mean=mean(PoolArea,na.rm=T),count = .N),by=.(PoolQC)]

df[c(2421,2504),"PoolQC"] = "Ex"
df[2600,"PoolQC"] = "Fa"
df[is.na(df$PoolQC),"PoolQC"] = 0