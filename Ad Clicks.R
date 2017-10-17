rm(list = ls())
setwd("F:\\Mach. Learning\\Kaggle case studies\\Predict Ad Clicks")

library(data.table)
library(caret)
library(xgboost)


dt = fread("train.csv")
test = fread("test.csv")
sapply(dt,function(x) sum(is.na(x)) + length(which(x=="")))
sapply(test,function(x) sum(is.na(x)) + length(which(x=="")))

### Merging  same levels...

dt[,browserid := ifelse(browserid %in% c("Chrome"),"Google Chrome",ifelse(browserid %in% c("Mozilla","Firefox"),"Mozilla Firefox",ifelse(browserid %in% c("IE","InternetExplorer"),"Internet Explorer",browserid)))]
dt[which(browserid == ""),browserid := "Others"]

dt[which(devid == ""),devid := "Others"]

rem = c("ID","datetime","siteid","offerid","merchant","category")
dt[,ID := NULL]
dt[,datetime := NULL]
dt[,siteid := NULL]
dt[,offerid := NULL]
dt[,merchant := NULL]
dt[,category := NULL]

dt[,datetime := as.POSIXct(strptime(datetime,"%Y-%m-%d %H:%M:%S"))]
dt[,DayOfWeek := weekdays(datetime)]
dt[,DayOfMonth := as.integer(format(datetime,"%d"))]
dt[,TimeOfTheDay := as.integer(format(datetime,"%H"))]
dt[,NightTime := ifelse(TimeOfTheDay %in% c(18:23,0,1),"Yes","No")]
dt[,WeekEnd := ifelse(DayOfWeek %in% c("Saturday","Sunday"),"Yes","No")]

offer_dt = dt[,.(offer_perc = mean(click)),.(offerid)]
merchant_dt = dt[,.(merch_perc = mean(click)),.(merchant)]
category_dt = dt[,.(category_perc = mean(click)),.(category)]

m1 = merge(dt,offer_dt,by = "offerid")
m2 = merge(dt,merchant_dt,by = "merchant")
m3 = merge(m2,category_dt,by = "category")
dt = m3

factor_vars = c("countrycode","browserid","devid","DayOfMonth","DayOfWeek","WeekEnd","TimeOfTheDay","NightTime")
dt[,(factor_vars) := lapply(.SD,as.factor),.SDcols = factor_vars]


downsample <- function(data,target){
	z_ind = which(data[,target,with = F]==0)
	o_ind = which(data[,target,with = F]==1)
	total = round((length(o_ind) * 100)/40)
	z_cnt = total - length(o_ind)
	z_dt = data[sample(z_ind,z_cnt,replace = F),]
	o_dt = data[o_ind,]
	final = rbind(z_dt,o_dt)
	return(final)
}
t = downsample(dt,"click")
dt_bkp = copy(dt)
dt = t
ind = createDataPartition(dt$click,p = 0.70 ,list = F)
train = dt[ind,]
valid = dt[-ind,]

library(Matrix)
dtrain = xgb.DMatrix(data = sparse.model.matrix(click~.-1,data=train),label=train$click)
dtest = xgb.DMatrix(data = sparse.model.matrix(click~.-1,data=valid),label=valid$click)

xgb <- xgb.train(data = dtrain,
	 nrounds =100, 
	 max_depth = 4,
	 eval_metric = "auc",
	 objective = "binary:logistic",
	 subsample = 1,
	 colsample_bytree = 1,
	 watchlist = list(eval=dtest,train = dtrain),
	 early.stop.round = 10,
	 print.every.n = 10
	)

valid$xgb_pred = predict(xgb,dtest)

### Logistic...

logFit <- glm(click~.,data = train, family = binomial)
summary(logFit)

valid$Log_pred = predict(logFit,valid,type = "response")

### Stacking using logistic model...

stack_fit = glm(click~xgb_pred + Log_pred,data = valid,family = binomial)
summary(stack_fit)


### Scoring....


test[,browserid := ifelse(browserid %in% c("Chrome"),"Google Chrome",ifelse(browserid %in% c("Mozilla","Firefox"),"Mozilla Firefox",ifelse(browserid %in% c("IE","InternetExplorer"),"Internet Explorer",browserid)))]
test[which(browserid == ""),browserid := "Others"]

test[which(devid == ""),devid := "Others"]

test[is.na(offer_perc)]

tm1 = merge(test,offer_dt,by = "offerid",all.x = TRUE)
tm2 = merge(tm1,merchant_dt,by = "merchant",all.x = TRUE)
tm3 = merge(test,category_dt,by = "category")

test = tm3
test[,datetime := as.POSIXct(strptime(datetime,"%Y-%m-%d %H:%M:%S"))]
test[,DayOfWeek := weekdays(datetime)]
test[,DayOfMonth := as.integer(format(datetime,"%d"))]
test[,TimeOfTheDay := as.integer(format(datetime,"%H"))]
test[,NightTime := ifelse(TimeOfTheDay %in% c(18:23,0,1),"Yes","No")]
test[,WeekEnd := ifelse(DayOfWeek %in% c("Saturday","Sunday"),"Yes","No")]

factor_vars = c("countrycode","browserid","devid","DayOfMonth","DayOfWeek","NightTime","WeekEnd")
test[,(factor_vars) := lapply(.SD,as.factor),.SDcols = factor_vars]

test_bkp = copy(test)

ID = test[,.(ID)]
test[,ID := NULL]
test[,datetime := NULL]
test[,siteid := NULL]
test[,offerid := NULL]
test[,merchant := NULL]
test[,category := NULL]

dtest1 = xgb.DMatrix(data = sparse.model.matrix(~.-1,data=test))
test[,DayOfMonth := NULL]
x1 = predict(xgb,dtest1)

x2 = predict(logFit,test,type = "response")
test$xgb_pred = x1
test$Log_pred = x2

x3 = predict(stack_fit,test,type = "response")
final = cbind(ID,click = x3)
avg = (x1 + l1)/2
final = cbind(ID,click = avg)
	