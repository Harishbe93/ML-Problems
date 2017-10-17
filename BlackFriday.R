rm(list=ls())
setwd("F:\\Mach. Learning\\Kaggle case studies\\Black Friday")

library(data.table)
dt = fread("train.csv")
test = fread("test.csv")

sapply(dt,function(x) sum(is.na(x)) + length(which(x=="")))


factor_cols = c("User_ID","Product_ID","Gender","Age","Occupation","City_Category","Product_Category_1","Stay_In_Current_City_Years","Marital_Status")
dt[,(factor_cols) := lapply(.SD,as.factor),.SDcols = factor_cols]
dt[,c("Product_Category_2","Product_Category_3") := lapply(.SD,function(x) ifelse(is.na(x),999,x)),.SDcols = c("Product_Category_2","Product_Category_3")]

# rem_cols = c("Product_Category_2","Product_Category_3")
# dt[,rem_cols := NULL , with = F]

dt = dt[!Product_Category_1 %in% c("19","20")]

### Feature Engineering...
user_dt = dt[,.(User_count = .N,User_mean = mean(Purchase),User_SD = sd(Purchase)),User_ID]
dt = merge(dt,user_dt,by = "User_ID")

prod_dt = dt[,.(Product_count = .N,Product_mean = mean(Purchase),Product_SD = sd(Purchase)),Product_ID]
dt = merge(dt,prod_dt,by = "Product_ID")

dt[,Diff_amount := abs(Product_mean - User_mean)]
dt[,Product_SD := ifelse(is.na(Product_SD),0,Product_SD)]

library(xgboost)
library(Matrix)

ind = sample(1:nrow(dt),round(0.70*nrow(dt)),replace = F)
train = dt[ind,]
valid = dt[-ind,]

dtrain = xgb.DMatrix(data = sparse.model.matrix(Purchase~.-1,data=train),label=train$Purchase)
dtest = xgb.DMatrix(data = sparse.model.matrix(Purchase~.-1,data=valid),label=valid$Purchase)

params <- list(booster = "gbtree", objective = "reg:linear", eta=0.3, gamma=0, max_depth=5, subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, print.every.n = 10, early.stop.round = 20, maximize = F)

	xgb <- xgb.train(data = dtrain,
	 nrounds =120, 
	 max_depth = 5,
	 objective = "reg:linear",
	 subsample = 1,
	 colsample_bytree = 1,
	 watchlist = list(eval=dtest,train = dtrain),
	 print.every.n = 10
	)
xgb.importance(colnames(train), model = fit, data = train, label = Purchase) 

names <- dimnames(data.matrix(train[,-Purchase]))[[2]]


#### Tuning hyperparameters....

dt1 = data.frame()
for(i in 1:10){

	params = list(booster = "gbtree",objective = "reg:linear",eta = 0.3, subsample = sample(seq(0.5,0.9,by = 0.01),1,replace = F),colsample_bytree = sample(seq(0.5,0.9,by = 0.01),1,replace = F),max_depth = 5)
	xgbcv <- xgb.train( params = params, data = dtrain, nrounds = 100, print.every.n = 10, watchlist = list(eval=dtest,train = dtrain),early.stop.round = 5, maximize = F)
	dt1 = rbind(dt1,data.frame(subsample = params$subsample , colsample_bytree = params$colsample_bytree,max_depth = params$max_depth))
}