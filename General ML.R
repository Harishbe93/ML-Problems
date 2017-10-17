
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(rep(c(1,2),100))
getModelPerf <- function(data,target,classification = TRUE){
	library(caret)
	library(caretEnsemble)
	library(xgboost)
	library(data.table)
	library(Matrix)
	library(randomForest)


	dt = data.table(data)
	names(dt)[which(names(dt)==target)] = "Target"
	
	### Data Pre - processing...
	print(summary(dt))
	print("Missing value treatment starts...")

	missing = sapply(dt,function(x) sum(is.na(x))+length(which(x=="")))
	missing = missing / nrow(dt) ### Missing Percentage...
	mis_cols = names(missing[missing<20 & missing > 0]) ### Remove features with missing more than 20%...
	rem_cols = names(missing[missing >= 20])
	if(length(rem_cols)!=0)
		dt = dt[,-rem_cols,with = FALSE]
	for(i in mis_cols){
		if(class(dt[[i]]) %in% c("integer","numeric"))
			dt[is.na(eval(parse(text=i))),eval(parse(text=i))] = median(dt[[i]],na.rm=TRUE)
		else
			dt[is.na(eval(parse(text=i))),eval(parse(text=i))] = Mode(dt[[i]])
	}

	print("Missing Value Handling is completed...")

	### Do stratified sampling...

	if(classification){
		obj = "binary:logistic"
		ind = createDataPartition(dt[,Target],p = 0.70,list = FALSE)
		train = dt[ind,]
		test = dt[-ind,]
	}
	else{
		obj = "reg:linear"
		ind = sample(c(1:nrow(dt)),round(0.70*nrow(dt)),replace=FALSE)
		train = dt[ind,]
		test = dt[-ind,]
	}

	### Random Forest...
	trainControl = trainControl(method = "cv", number = 5)
	gbmFit = train(Target~.,data = train, method = "gbm",trControl = trainControl)
	gbmAcc = getMetric(gbmFit)
	
	ntree = 500
	rfFit = randomForest(Target~.,data = train,ntree = 500)
	rfAcc = as.numeric(1 - rfFit$err.rate[ntree,1])
	
	dtrain = xgb.DMatrix(data = sparse.model.matrix(Target~.-1,data=train),label=train$Target)
	dtest = xgb.DMatrix(data = sparse.model.matrix(Target~.-1,data=test),label=test$Target)

	xgbFit <- xgb.train(data = dtrain, 
			 nrounds = 150, 
			 objective = obj,
			 subsample = 1,
			 colsample_bytree = 1,
			 watchlist = list(eval=dtest,train = dtrain),
			 print.every.n = 10
			)
	
	models = list()
	models[[1]] = gbmFit
	models[[2]] = rfFit
	models[[3]] = data.frame(Model = c("Gradient Boosting Machine","Random Forest"),Accuracy = c(gbmAcc,rfAcc))
	#models[[3]] = xgbFit
	return(models)
}