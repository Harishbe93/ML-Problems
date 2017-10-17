rm(list = ls())
setwd("F:\\Mach. Learning\\Kaggle case studies\\ZS Young Data Scientist")

library(data.table)
dt = fread("train.csv")
test = fread("test.csv")

sapply(dt, function(x) sum(is.na(x)) + length(which(x== ""))) ### No missing values...

dt <- dt[order(PID)]
test <- test[order(PID)]


dt[,c("PID","Event") := lapply(.SD,as.factor),.SDcols = c("PID","Event")]
dt[,Date := paste(Date,"01",sep="")]
dt[,Date := as.Date(Date,"%Y%m%d")]

dt[,Month := format(Date,"%m")]

patient_tbl = dt[,.(cnt = .N),.(PID,Event)]

merge(dt,patient_tbl,by = c("PID","Event"))

patient_tbl = dt[,.(No_of_Visits = .N),PID]
patient_tbl[,Duration := as.integer((maxDate - minDate)/30)]






# Starter code

		train_dcast <- dcast(data = train, PID ~ Event, length, value.var = "Event")
		random_submit <- colnames(train_dcast)[-1][apply(train_dcast[,-c('PID'),with=F],1, function(x)order(-x)[1:10])]
		random_mat <- as.data.table((matrix(random_submit,ncol = 10, byrow = T)))
		colnames(random_mat) <- colnames(sample_sub)[-1]
		random_mat <- cbind(PID = test$PID, random_mat)
		fwrite(random_mat,"random_sub.csv")




### Model for each event...

models = list()
k = 1
for(i in unique(dt$PID)){

temp = dt[PID=="1002810"]
temp[,Month := NULL]
ind = sample(1:nrow(temp),round(.7 * nrow(temp)),replace = F)
train = temp[ind,]
valid = temp[-ind,]

dtrain = xgb.DMatrix(data = sparse.model.matrix(Event~.-1,data=train1),label=train1$Event)
dtest = xgb.DMatrix(data = sparse.model.matrix(Event~.-1,data=valid),label=valid$Event)


xgb <- xgb.train(data = dtrain,
	 nrounds =50, 
	 max_depth = 3,
	 objective = "binary:logistic",
	 subsample = 1,
	 colsample_bytree = 1,
	 watchlist = list(eval=dtest,train = dtrain),
	 print.every.n = 10
	)
models[[k]] = xgb
k = k + 1
}