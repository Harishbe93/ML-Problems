rm(list=ls())
setwd("F:\\Mach. Learning\\Kaggle case studies\\Einsite")

### Loading all the libraries....
library(ggplot2)
library(caret)
library(xgboost)
library(data.table)

### Read the train dataset..
data = fread("cabbie_datafa2fec8\\train.csv")

dt = data

dt[,vendor_id := as.factor(vendor_id)]
dt[,payment_type := as.factor(payment_type)]
dt[,new_user := as.factor(new_user)]
dt[,store_and_fwd_flag := as.factor(store_and_fwd_flag)]
dt[,rate_code := as.factor(as.character(rate_code))]



data = fread("Data for Modeling.csv")
data = data.frame(data)
data$vendor_id = as.factor(data$vendor_id)
data$new_user = as.factor(data$new_user)
data$store_and_fwd_flag = as.factor(data$store_and_fwd_flag)
data$payment_type = as.factor(data$payment_type)
data$rate_code = as.factor(as.character(data$rate_code))

a=apply(data,1,function(x) sum(is.na(x))+length(which(x=="")))
data$misses = a

data_bkp = data


data = data[data$misses==0,]

dist1 <- function(x){
	a=as.numeric(x[c("pickup_longitude","pickup_latitude")])
	b=as.numeric(x[c("dropoff_longitude","dropoff_latitude")])
	return(distm(a,b,fun=distHaversine))
}

library(geosphere)
data$Distance = 0
for(i in 1:nrow(data)){
	data$Distance[i] = dist(data[i,])
}

data$Distance = apply(data,1,function(x) dist1(x))
### Exploratory analysis...

sapply(data,function(x) sum(is.na(x)))
sapply(data,function(x) length(which(x=="")))

data$TID = NULL ## Dropping the unique column
data[which(data$new_user==""),"new_user"] = "YES" ## Filling with the mode "NO" for this feature as it contains only 19 missing values...

## tip amount has an outlier 231.20...replacing that with 99th percentile
m = max(data$tip_amount,na.rm=T)
data[which(data$tip_amount==m),"tip_amount"] = quantile(data$tip_amount,0.99,na.rm=T)
data = data.table(data)
temp1 = data[,.(mean = mean(tip_amount,na.rm=T)),.(rate_code)]
temp2 = data[is.na(tip_amount),]
m = merge(temp1,temp2,by.x = c("rate_code"),by.y = c("rate_code"))
m$tip_amount = m$mean
m$mean = NULL
temp3 = data[!is.na(tip_amount),]
data1 = rbind(temp3,m)

#data[is.na(data$tip_amount),"tip_amount"] = median(data$tip_amount,na.rm=T)

data = data1
temp1 = data[,.(mean = mean(surcharge,na.rm=T)),.(payment_type)]
temp2 = data[is.na(surcharge),]
m = merge(temp1,temp2,by.x = c("payment_type"),by.y = c("payment_type"))
m$surcharge = m$mean
m$mean = NULL
temp3 = data[!is.na(surcharge),]
data1 = rbind(temp3,m)
data = data1

#data[is.na(data$surcharge),"surcharge"] = mean(data$surcharge,na.rm=T)
data[which(data$store_and_fwd_flag==""),"store_and_fwd_flag"] = "N"  #### Has some pattern with surcharge

### Not using lat and long as of now...

data$pickup_longitude = data$pickup_latitude = data$dropoff_longitude = data$dropoff_latitude = NULL
data = data.frame(data)
### Calculate the starting hour of the journey...
data$pickup_datetime = strptime(data$pickup_datetime,"%Y-%m-%d %H:%M:%S")
data$Hour = format(data$pickup_datetime,"%H")
data$Hour = as.numeric(data$Hour)

### Calculate journey time...
data$dropoff_datetime = strptime(data$dropoff_datetime,"%Y-%m-%d %H:%M:%S")
data$Journey_time = difftime(data$dropoff_datetime,data$pickup_datetime, unit="mins")
data$Journey_time = as.numeric(data$Journey_time)
#data$pickup_datetime = data$dropoff_datetime = NULL
data$rate_code = as.factor(data$rate_code)

### Calculate day of travel
#data = cbind(data,data_bkp["pickup_datetime"])
data$pickup_datetime = strptime(data$pickup_datetime,"%Y-%m-%d %H:%M:%S")
data$Date = format(data$pickup_datetime,"%Y-%m-%d")
data$Date = as.Date(data$Date,"%Y-%m-%d")
data$DayOfWeek = weekdays(data$Date)
data$DayOfWeek = as.factor(data$DayOfWeek)
data$StartDay = weekdays(data$Date)	
data$Date = format(data$dropoff_datetime,"%Y-%m-%d")
data$Date = as.Date(data$Date,"%Y-%m-%d")
data$EndDay = weekdays(data$Date)
data$OneDay = as.numeric(data$StartDay==data$EndDay)
data$EndDay =NULL
data$StartDay = NULL
data$Date = data$pickup_datetime = data$dropoff_datetime = NULL
data$OneDay = as.factor(as.character(data$OneDay))

data$Weekend = ifelse(data$DayOfWeek %in% c("Sunday","Saturday"),"Yes","No")
data$Weekend = as.factor(data$Weekend)
data$TypeOfTravel = ifelse(data$Hour %in% c(0:5,22:24),"Peak","Non - peak")
data$TypeOfTravel = as.factor(data$TypeOfTravel)

data$PassengerType = ifelse(data$passenger_count %in% c(0:2),"Small",ifelse(data$passenger_count %in% c(3:6),"Medium","Large"))
data$PassengerType = as.factor(data$PassengerType)
#data$Weekend=NULL

data$new = ifelse(any(data$rate_code %in% c("0","1","2","3","4","5")),data$rate_code,"111")

fit = readRDS("Linear.rds")
GBTFit = readRDS("GBT.rds")
DTFit = readRDS("DT.rds")


### Function to calcualte MAE..

mae <- function(predicted,test_data){
	test_data$Prediction = predicted
	test_data$AbsDiff = abs(test_data$Prediction - test_data$fare_amount)
	print("MAE :")
	print(sum(test_data$AbsDiff)/nrow(test_data))
}


# data$DayOfWeek = NULL
# data$Hour = NULL
ind = sample(1:nrow(data),round(0.7*nrow(data)),replace=F)
train = data[ind,]
test = data[-ind,]
fit = lm(fare_amount~.-Weekend,data = train)
summary(fit)

### Checking error of Linear model...
f = predict(fit,test)
# test$Prediction = f
# test$AbsDiff = abs(test$fare_amount - test$Prediction)
# sum(test$AbsDiff)/nrow(test)
mae(f,test)

### Machine Learning models...
library(caret)
trainControl = trainControl(method = 'cv',number = 5)

gbmGrid <-  expand.grid(interaction.depth = c(1:3),
                    n.trees = (3:5)*50, 
                    shrinkage = c(.01, .1,.15),
                    n.minobsinnode = 10) 

param = data.frame(shrinkage = 0.1,n.minobsinnode = 10 , interaction.depth = 3,n.trees = 150)
GBTFit1 = train(fare_amount~.,data =  train[sample(1:nrow(train),20000,replace=F),], method = 'gbm',verbose = F,trControl = trainControl,tuneGrid = param)  

f = predict(GBTFit1,test)
test$Prediction = f
test$AbsDiff = abs(test$fare_amount - test$Prediction)
sum(test$AbsDiff)/nrow(test)


rf1 = randomForest(fare_amount~.,data=train[sample(1:nrow(train),5000,replace=F),],ntree=100)

rf2 = randomForest(fare_amount~.,data=train[sample(1:nrow(train),5000,replace=F),],ntree=100)

rf3 = randomForest(fare_amount~.,data=train[sample(1:nrow(train),5000,replace=F),],ntree=100)

rf4 = randomForest(fare_amount~.,data=train[sample(1:nrow(train),5000,replace=F),],ntree=100)

rf5 = randomForest(fare_amount~.,data=train[sample(1:nrow(train),5000,replace=F),],ntree=100)

rf6 = randomForest(fare_amount~.,data=train[sample(1:nrow(train),5000,replace=F),],ntree=100)



rf2 = randomForest(fare_amount~.,data=train[40001:50000,],ntree=10)

rf3 = randomForest(fare_amount~.,data=train[100001:110000,],ntree=10)

rf4 = randomForest(fare_amount~.,data=train[400001:410000,],ntree=10)

rf5 = randomForest(fare_amount~.,data=train[900001:911000,],ntree=10)


f1 = predict(rf1,test)
f2 = predict(rf2,test)
f3 = predict(rf3,test)
f4 = predict(rf4,test)
f5 = predict(rf5,test)
f6 = predict(rf6,test)

p = (f1+f2+f3+f4+f5+f6)/6

rf3_f = predict(rf3,test)
mae(p,test)
rf4_f = predict(rf4,test)
rf5_f = predict(rf5,test)


param = data.frame(cp=0.01)
DTFit = train(fare_amount~.,data =  train,method = 'rpart',trControl=trainControl,tuneGrid = param)

f = predict(DTFit,test)
test$Prediction = f
test$AbsDiff = abs(test$fare_amount - test$Prediction)
sum(test$AbsDiff)/nrow(test)

ada_fit = train(fare_amount~., data=train[sample(1:nrow(train),100000,replace=F),], method="ada",trControl=trainControl)

f = predict(rf,test)
test$Prediction = f
test$AbsDiff = abs(test$fare_amount - test$Prediction)
sum(test$AbsDiff)/nrow(test)
### Checking the MAE in our test dataset...
f1 = predict(fit,test)
f2 = predict(GBTFit,test)
f3 = predict(DTFit,test)
p = (f1+f2+f3+f4+f)/5
mae(p,test)


t1 = predict(fit,train)
t2 = predict(GBTFit,train)
t3 = predict(DTFit,train)
t = (t1+t2+t3)/3
mae(t,train)

train$m1 = t1
train$m2 = t2
train$m3 = t3
fit = lm(fare_amount~.-Weekend,data = train)
summary(fit)



data_bkp = data
data=cbind(data,t1,t2,t3)
ind = sample(1:nrow(data),round(0.7*nrow(data)),replace=F)
train1 = data[ind,]
test1 = data[-ind,]



dtrain = xgb.DMatrix(data = as.matrix(train1[,c("t1","t2","t3")]),label = as.numeric(train1$fare_amount))
dtest = xgb.DMatrix(data = as.matrix(test1[,c("t1","t2","t3")]),label = as.numeric(test1$fare_amount))
xgb <- xgb.train(data = dtrain, 
 label = "fare_amount", 
 max_depth = 15, 
 nrounds =200, 
 subsample = 0.5,
 colsample_bytree = 0.5,
 seed = 1,
 eval_metric = "rmse",
 watchlist = list(eval=dtest,train = dtrain)
)




SVMFit <- train(fare_amount ~ ., data = train[sample(1:nrow(train),100000,replace=F),],
    method = "svmRadial",tuneLength=1,trControl = trainControl)    
f = predict(SVMFit,test)
test$Prediction = f
test$AbsDiff = abs(test$fare_amount - test$Prediction)
sum(test$AbsDiff)/nrow(test)


### For the actual test data provided...

test2= read.csv("cabbie_datafa2fec8/test.csv")
sapply(test2,function(x) sum(is.na(x)))
sapply(test2,function(x) length(which(x=="")))

temp1 = tdt[,.(mean = mean(tip_amount,na.rm=T)),.(rate_code)]
temp2 = tdt[is.na(tip_amount),]
m = merge(temp1,temp2,by.x = c("rate_code"),by.y = c("rate_code"))
m$tip_amount = m$mean
m$mean = NULL
temp3 = tdt[!is.na(tip_amount),]
tdt = rbind(temp3,m)



test2[is.na(test2$tip_amount),"tip_amount"] = median(test2$tip_amount,na.rm=T)

temp1 = tdt[,.(mean = mean(tip_amount,na.rm=T)),.(rate_code)]
temp2 = tdt[is.na(tip_amount),]
m = merge(temp1,temp2,by.x = c("rate_code"),by.y = c("rate_code"))
m$tip_amount = m$mean
m$mean = NULL
temp3 = tdt[!is.na(tip_amount),]
tdt = rbind(temp3,m)



test2[is.na(test2$surcharge),"surcharge"] = mean(test2$surcharge,na.rm=T)


test2[which(test2$new_user==""),"new_user"] = "NO" ## Filling with the mode "NO" for this feature as it contains only 19 missing values...
test2[which(test2$store_and_fwd_flag==" "),"store_and_fwd_flag"] = "N" ## Filling with the mode "NO" for this feature as it contains only 19 missing values...
test2$store_and_fwd_flag = as.factor(as.character(test2$store_and_fwd_flag))
test2$pickup_datetime = strptime(test2$pickup_datetime,"%Y-%m-%d %H:%M:%S")
test2$dropoff_datetime = strptime(test2$dropoff_datetime,"%Y-%m-%d %H:%M:%S")
test2$Journey_time = difftime(test2$dropoff_datetime,test2$pickup_datetime, unit="mins")
test2$Journey_time = as.numeric(test2$Journey_time)
test2$rate_code = as.factor(test2$rate_code)
test2$Hour = format(test2$pickup_datetime,"%H")
test2$Hour = as.numeric(test2$Hour)
test2$Date = format(test2$pickup_datetime,"%Y-%m-%d")
test2$Date = as.Date(test2$Date,"%Y-%m-%d")
test2$StartDay = weekdays(test2$Date)	
test2$Date = format(test2$dropoff_datetime,"%Y-%m-%d")
test2$Date = as.Date(test2$Date,"%Y-%m-%d")
test2$EndDay = weekdays(test2$Date)
test2$OneDay = as.numeric(test2$StartDay==test2$EndDay)
test2$OneDay = as.factor(as.character(test2$OneDay))
test2$DayOfWeek = weekdays(test2$Date)
test2$DayOfWeek = as.factor(test2$DayOfWeek)
test2$Date = test2$pickup_datetime = NULL
test2$Weekend = ifelse(test2$DayOfWeek %in% c("Sunday","Saturday"),"Yes","No")
test2$Weekend = as.factor(test2$Weekend)
test2$TypeOfTravel = ifelse(test2$Hour %in% c(0:5,22:24),"Peak","Non - peak")
test2$TypeOfTravel = as.factor(test2$TypeOfTravel)
test2$Distance = apply(test2,1,function(x) dist1(x))


test1$Weekend=NULL


f1 = predict(fit,test2)
f2 = predict(GBTFit,test2)
f3 = predict(DTFit,test2)
xgb_data = cbind(f1,f2,f3)
xgb_test = xgb.DMatrix(data = as.matrix(xgb_data))

final = predict(xgb,xgb_test)


test1$fare_amount = final
sub = test1[,c("TID","fare_amount")]
write.csv(sub,"Sample Submission10.csv",row.names = F)


val_set = test[sample(1:nrow(test),100000,replace=F),]
val_mat = sparse.model.matrix(fare_amount~.-1,data = val_set)

samp_train = train[sample(1:nrow(train),300000,replace=F),]
samp_test = test[sample(1:nrow(test),100000,replace=F),]


train_mat = sparse.model.matrix(fare_amount~.-1,data = samp_train )
test_mat = sparse.model.matrix(fare_amount~.-1,data = samp_test)


dtrain = xgb.DMatrix(data = as.matrix(train_mat),label = as.numeric(samp_train$fare_amount))
dtest = xgb.DMatrix(data = as.matrix(test_mat),label = as.numeric(samp_test$fare_amount))
xgb1<- xgb.train(data = dtrain, 
 label = "fare_amount", 
 max_depth = 5, 
 nrounds =120, 
 subsample = 0.5,
 colsample_bytree = 0.5,
 eval_metric = "rmse",
 watchlist = list(eval=dtest,train = dtrain),
 print.every.n = 10
)

f1 = predict(xgb3,val_mat)
f2 = predict(xgb2,val_mat)
f3 = predict(xgb1,val_mat)
f4 = predict(xgb4,val_mat)
p = (f1+f2+f3+f4)/4
mae(p,val_set)


rf = randomForest(fare_amount~.,data = train1[sample(1:nrow(train1),10000,replace=F),] , ntree=120)

s1 = predict(fit,test1)
test1$Prediction = s1
test1$AbsDiff = abs(test1$fare_amount - test1$Prediction)
sum(test1$AbsDiff)/nrow(test1)

f = predict(xgb4,val_mat)
val_set$Prediction = f
val_set$AbsDiff = abs(val_set$fare_amount - val_set$Prediction)
sum(val_set$AbsDiff)/nrow(val_set)

f = predict(xgb2,test_mat)
samp_test$Prediction = f
samp_test$AbsDiff = abs(samp_test$fare_amount - samp_test$Prediction)
sum(samp_test$AbsDiff)/nrow(samp_test)

test1_bkp = test1
test1$TID = test1$dropoff_datetime = test1$pickup_datetime = test1$pickup_latitude = test1$pickup_longitude = test1$dropoff_latitude = test1$dropoff_longitude = test1$StartDay = test1$EndDay = NULL
test_mat = xgb.DMatrix(data = sparse.model.matrix(~.-1,data = test1))

dtest1 = xgb.DMatrix(dtest1 = sparse.model.matrix(fare_amount~.-1,data=test1),label=as.numeric(train$fare_amount))

### Stacking....

test = cbind(test,f1,f2,f3)
ind = sample(1:nrow(test),round(0.7*nrow(test)),replace=F)
train1 = test[ind,]
test1 = test[-ind,]
fit1 = lm(fare_amount~f1+f2+f3,data = train1)
summary(fit1)

s1 = predict(fit1,test1)
test1$Prediction = s1
test1$AbsDiff = abs(test1$fare_amount - test1$Prediction)
sum(test1$AbsDiff)/nrow(test1)






	dtrain = xgb.DMatrix(data = as.matrix(train1[,c("f1","f2","f3")]),label = as.numeric(train1$fare_amount))
	dtest = xgb.DMatrix(data = as.matrix(test1[,c("f1","f2","f3")]),label = as.numeric(test1$fare_amount))
		xgb <- xgb.train(data = dtrain, 
		 label = "fare_amount", 
		 max_depth = 10, 
		 nrounds =150, 
		 subsample = 1,
		 colsample_bytree = 0.8,
		 eval_metric = "rmse",
		 watchlist = list(eval=dtest,train = dtrain),
		 print.every.n = 10
		)

s3 = predict(xgb,dtest)
test1$Prediction = s3
test1$AbsDiff = abs(test1$fare_amount - test1$Prediction)
sum(test1$AbsDiff)/nrow(test1)

param = data.frame(shrinkage = 0.1,n.minobsinnode = 10 , interaction.depth = 3,n.trees = 100)

GBTFit1 = train(fare_amount~f1+f2+f3,data =  train1[sample(1:nrow(train1),100000,replace=F),], method = 'gbm',verbose = F,trControl = trainControl,tuneGrid = param)  

s2 = predict(GBTFit1,test1)
test1$Prediction = s2
test1$AbsDiff = abs(test1$fare_amount - test1$Prediction)
sum(test1$AbsDiff)/nrow(test1)

### Averaging...
s_final = (s1+s2+s3)/3
test1$Prediction = s_final
test1$AbsDiff = abs(test1$fare_amount - test1$Prediction)
sum(test1$AbsDiff)/nrow(test1)



test1 = cbind(test1,s1,s3)
ind = sample(1:nrow(test1),round(0.7*nrow(test1)),replace=F)
train2 = test1[ind,]
test2 = test1[-ind,]
fit2 = lm(fare_amount~s1+s3,data = train2)
summary(fit2)


dtrain = xgb.DMatrix(data = as.matrix(train2[,c("s1","s3")]),label = as.numeric(train2$fare_amount))
dtest = xgb.DMatrix(data = as.matrix(test2[,c("s1","s3")]),label = as.numeric(test2$fare_amount))
xgb <- xgb.train(data = dtrain, 
 label = "fare_amount", 
 max_depth = 10, 
 nrounds =150, 
 subsample = 1,
 colsample_bytree = 0.8,
 eval_metric = "rmse",
 watchlist = list(eval=dtest,train = dtrain),
 print.every.n = 10
)

s3 = predict(fit2,test2)
test2$Prediction = s3
test2$AbsDiff = abs(test2$fare_amount - test2$Prediction)
sum(test2$AbsDiff)/nrow(test2)
