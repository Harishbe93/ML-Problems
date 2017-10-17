rm(list = ls())
gc()


setwd("F:\\Mach. Learning\\Kaggle case studies\\New York City Taxi Trip Duration")

library(data.table)
library(ggplot2)
library(leaflet)
library(xgboost)
library(caret)

train = fread("train.csv")
test = fread("test.csv")
summary(train)
summary(test)

### Checking for missing values in the datasets..

print(paste("The total missing values in train is",sum(is.na(train)),sep = " "))
print(paste("The total missing values in test is",sum(is.na(test)),sep = " "))

### Converting to factors and dates...
factor_vars = c("vendor_id","store_and_fwd_flag")
date_vars = c("pickup_datetime","dropoff_datetime")
train[,(factor_vars) := lapply(.SD,as.factor),.SDcols = factor_vars]
train[,(date_vars) := lapply(.SD,function(x) as.POSIXct(x,format = "%Y-%m-%d %H:%M:%S")),.SDcols = date_vars]

date_vars = "pickup_datetime"
test[,(factor_vars) := lapply(.SD,as.factor),.SDcols = factor_vars]
test[,(date_vars) := lapply(.SD,function(x) as.POSIXct(x,format = "%Y-%m-%d %H:%M:%S")),.SDcols = date_vars]


### Some features...
train[,Hour := as.integer(format(pickup_datetime,"%H"))]
train[,WeekDay := weekdays(pickup_datetime)]
train[,Weekend := ifelse(WeekDay %in% c("Saturday","Sunday"),"Yes","No")]
train[,Month := as.factor(format(pickup_datetime,"%b"))]

### Mapping the trips in NewYork
sample = sample(1:nrow(train),5000,replace = F)
leaflet(data = train[sample,]) %>% addTiles() %>% addMarkers(~pickup_longitude, ~pickup_latitude, popup = ~as.character(vendor_id), label = ~as.character(trip_duration))

### Exploratory Analysis...



ggplot(train,aes(trip_duration))+geom_histogram(bins = 150,fill = "red")+scale_y_sqrt()+scale_x_log10()
ggsave("Plots/Trip duration.png")

ggplot(train,aes(pickup_datetime))+geom_histogram(bins = 100,fill = "blue")
ggsave("pickup date.png")

ggplot(train,aes(dropoff_datetime))+geom_histogram(bins = 100,fill = "red")
ggsave("dropoff date.png")

ggplot(train,aes(vendor_id))+geom_bar()
ggsave("vendor id.png")

ggplot(train,aes(factor(passenger_count),fill = factor(passenger_count)))+geom_bar()+theme(legend.position = "none")


ggplot(train,aes(pickup_latitude))+geom_histogram(fill = "blue",bins = 100)

ggplot(train,aes(store_and_fwd_flag))+geom_bar(fill = "brown")
ggplot(train[,.N,WeekDay],aes(WeekDay,N, fill = WeekDay))+geom_line()
