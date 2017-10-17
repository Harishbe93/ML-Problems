rm(list=ls())
setwd("F:\\Mach. Learning\\Kaggle case studies\\Loan Prediction")

library(data.table)
library(ggplot2)
library(caret)
library(xgboost)
library(Matrix)

## Read the input data

df = fread("train_u6lujuX_CVtuZ9i.csv")
test = fread("test_Y3wMUE5_7gLdaTN.csv")

### Individual Features..

sapply(df,function(x) sum(is.na(x))+length(which(x=="")))

ggplot(df,aes(Loan_Status, fill = Loan_Status))+geom_bar()

df[which(Gender==""),Gender := "Male"] ## Mode...
df[,Gender := as.factor(Gender)]
ggplot(df,aes(Gender,fill = Loan_Status))+geom_bar()


df[which(Married==""),Married := "Yes"] ## Mode...
df[,Married := as.factor(Married)]
ggplot(df,aes(Married,fill = Loan_Status))+geom_bar()



df[which(Dependents==""),Dependents := "0"]
df[,Dependents := as.factor(Dependents)]
ggplot(df,aes(Dependents, fill = Loan_Status))+geom_bar()


df[which(Self_Employed==""),Self_Employed := "No"]
df[,Self_Employed := as.factor(Self_Employed)]
ggplot(df,aes(Self_Employed,fill = Loan_Status))+geom_bar()

df[,Education := as.factor(Education)]
ggplot(df,aes(Education,fill = Loan_Status))+geom_bar()

ggplot(df,aes(fill = Loan_Status,ApplicantIncome))+geom_histogram()
ggplot(df,aes(fill = Loan_Status,CoapplicantIncome))+geom_histogram()

m = median(df$LoanAmount,na.rm = T)
df[is.na(LoanAmount),LoanAmount := m]
ggplot(df,aes(LoanAmount, fill = Loan_Status))+geom_histogram()

ggplot(df,aes(Property_Area,fill = Loan_Status))+geom_bar()
df[is.na(Loan_Amount_Term),Loan_Amount_Term := 360] ### Mode..
ggplot(df,aes(Loan_Amount_Term))+geom_histogram()

df[is.na(Credit_History),Credit_History := 1] ### Mode..
df[,Credit_History := as.factor(Credit_History)]
ggplot(df,aes(Credit_History,fill = Loan_Status))+geom_bar()


###Modeling...

df[,Loan_Status := ifelse(Loan_Status =="Y",1,0)]
df[,Loan_Status := as.factor(Loan_Status)]
df[,Loan_ID := NULL]
ind = createDataPartition(df$Loan_Status,p = 0.70, list = F)
train = df[ind,]
valid = df[-ind,]
fit = glm(Loan_Status~.,data = train, family = binomial)

trControl = trainControl(method = "cv", number = 5)
fit = train(Loan_Status~.,data = train, method = "glm",trControl = trControl)




test[which(Gender==""),Gender := "Male"] ## Mode...
test[which(Married==""),Married := "Yes"] ## Mode...
test[which(Dependents==""),Dependents := "0"]
test[which(Self_Employed==""),Self_Employed := "No"]
test[is.na(LoanAmount),LoanAmount := m]
test[is.na(Loan_Amount_Term),Loan_Amount_Term := 360] ### Mode..
test[is.na(Credit_History),Credit_History := "1"] ### Mode..
test[,Loan_ID:=NULL]
test[,Gender:=as.factor(Gender)]
test[,Married := as.factor(Married)]
test[,Dependents := as.factor(Dependents)]
test[,Education := as.factor(Education)]
test[,Self_Employed := as.factor(Self_Employed)]
test[,Credit_History:= as.factor(Credit_History)]
test[,Property_Area:= as.factor(Property_Area)]

p = predict(fit,test)

sapply(test,function(x) sum(is.na(x))+length(which(x=="")))

rf = randomForest(Loan_Status~.,data = df)

# df$Loan_Status = as.factor(df$Loan_Status)
# ind = createDataPartition(df$Loan_Status,p=0.7,list=F)
# train = df[ind,]
# valid = df[-ind,]

# trainControl = trainControl(method = 'cv',number = 5)
# trainControl = trainControl(method="cv",number = 5,savePredictions = "final",classProbs = TRUE)
# algorithms <- c("rpart","gbm","rf")
# models <- caretList(Loan_Status~.,data = train,methodList = algorithms,trControl = trainControl)
# greedy_ensemble <- caretEnsemble(
#   models, 
#   metric="Accuracy",
#   trControl=trainControl(
#     number=2,
#     classProbs = TRUE
#     ))
# summary(greedy_ensemble)

# p = predict(gbm_ensemble,test1,type="prob")
# test1$final_status = ifelse(p>=0.5,1,0)
# write.csv(test1[,.(project_id,final_status)],"Submission8.csv",row.names=F)

# gbm_ensemble <- caretStack(models,method = "gbm",metric="Accuracy",
#   trControl=trainControl(
#     number=2,
#     classProbs=TRUE,
#     verbose = FALSE
#     ))
# summary(gbm_ensemble)
# train[,Loan_Status := as.factor(Loan_Status)]
# RfFit = train(Loan_Status~.,data=train,method="rf",trControl = trainControl) 
# knnFit = train(Loan_Status~.,data = train,method = "knn",trControl = trainControl,tuneLength = 10)
# DTFit = train(Loan_Status~.,data = train,method = "rpart",trControl = trainControl,tuneLength=10)
# nnFit = train(Loan_Status~.,data = train,method = "nnet",trControl = trainControl)
# GBTFit = train(Loan_Status~.,data = train,method = "gbm",trControl = trainControl,tuneLength=9)
# LogFit = train(Loan_Status~.,data = train,method = "glm",family="binomial",trControl = trainControl)
# SvmFit = train(Loan_Status~.,data=train,method="svmRadial",trControl = trainControl)
# SvmLinFit = train(Loan_Status~.,data=train,method="svmLinear",trControl = trainControl)
# p1 = predict(RfFit,test)
# p2 = predict(DTFit,test)
# p3 = predict(nnFit,test)
# p4 = predict(GBTFit,test)
# p5 = predict(SvmFit,test)
# p6 = predict(SvmLinFit,test)

# head(cbind(p1,p2,p3,p4,p5))

# res = data.frame(p1,p2,p3,p4,p5,p6)
# Mode <- function(x) {
#   ux <- unique(x)
#   return(ux[which.max(tabulate(match(x, ux)))][1,1])
# }


df_bkp = df
df[,Loan_Status := ifelse(Loan_Status=="Y",1,0)]
df[,Loan_ID := NULL]
ind = createDataPartition(df$Loan_Status,p=0.7,list=F)
train = df[ind,]
valid = df[-ind,]
dtrain = xgb.DMatrix(data = sparse.model.matrix(Loan_Status~.-1,data=train),label=train$Loan_Status)
dtest = xgb.DMatrix(data = sparse.model.matrix(Loan_Status~.-1,data=test),label=test$Loan_Status)

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)


xgb <- xgb.train(data = dtrain,
	 max_depth = 4,
	 nrounds =5, 
	 booster = "gbtree",
	 objective = "binary:logistic",
	 subsample = 1,
	 colsample_bytree = 1,
	 watchlist = list(eval=dtest,train = dtrain),
	 print.every.n = 1
	)


table(p1,test$Loan_Status)

test1  = fread("test_Y3wMUE5_7gLdaTN.csv")

test1[,Married := as.factor(Married)]
test1[,Gender := as.factor(Gender)]

test1[,Dependents := as.factor(Dependents)]
test1[,Education := as.factor(Education)]
test1[,Self_Employed := as.factor(Self_Employed)]
test1[,Credit_History:= as.factor(Credit_History)]
test1[,Property_Area:= as.factor(Property_Area)]

test1[which(test1$Gender==""),"Gender"] = "Male" ## Mode...
test1[which(test1$Dependents==""),"Dependents"] = "0"
test1[which(test1$Self_Employed==""),"Self_Employed"] = "No"
test1[is.na(test1$LoanAmount),"LoanAmount"] = median(test1$LoanAmount,na.rm=T)

test1[is.na(test1$Loan_Amount_Term),"Loan_Amount_Term"] = 360 ### Mode..
test1[is.na(test1$Credit_History),"Credit_History"] = "1" ### Mode..

p = predict(greedy_ensemble,test1)
test1$Loan_Status = p
test1[,Loan_ID := NULL]
dtest1 = xgb.DMatrix(data = sparse.model.matrix(~.-1,data=test1))
x1 = predict(xgb,dtest1)
x1 = as.integer(x1>=0.5)
test1$Loan_ID = ID
test1$Loan_Status = x1
write.csv(test1[,c("Loan_ID","Loan_Status")],"Submission6.csv",row.names=F)

	best_param = list()
	best_seednumber = 1234
	best_logloss = Inf
	best_logloss_index = 0

for (iter in 1:100) {
    param <- list(objective = "multi:softprob",
          eval_metric = "mlogloss",
          num_class = 12,
          max_depth = sample(6:10, 1),
          eta = runif(1, .01, .3),
          gamma = runif(1, 0.0, 0.2), 
          subsample = runif(1, .6, .9),
          colsample_bytree = runif(1, .5, .8), 
          min_child_weight = sample(1:40, 1),
          max_delta_step = sample(1:10, 1)
          )
    cv.nround = 1000
    cv.nfold = 5
    seed.number = sample.int(10000, 1)[[1]]
    set.seed(seed.number)
    mdcv <- xgb.cv(data=dtrain, params = param, nthread=6, 
                    nfold=cv.nfold, nrounds=cv.nround,
                    verbose = T, early.stop.round=8, maximize=FALSE)

    min_logloss = min(mdcv[, test.mlogloss.mean])
    min_logloss_index = which.min(mdcv[, test.mlogloss.mean])

    if (min_logloss < best_logloss) {
        best_logloss = min_logloss
        best_logloss_index = min_logloss_index
        best_seednumber = seed.number
        best_param = param
    }
}

nround = best_logloss_index
set.seed(best_seednumber)
md <- xgb.train(data=dtrain, params=best_param, nrounds=nround, nthread=6)


xgb_tuning <- function(train, valid, target, classification = TRUE){

	dtrain = xgb.DMatrix(data = sparse.model.matrix(formula(paste(target,"~. -1",sep = "")),data = train),label = train[[target]])
	dvalid = xgb.DMatrix(data = sparse.model.matrix(formula(paste(target,"~. -1",sep = "")),data = valid),label = valid[[target]])

	### placeholder for best_parameters...
	best_params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=0, min_child_weight=0, subsample=0, colsample_bytree=0)



	### Perform a CV to decide on the number of rounds...
	params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=5, min_child_weight=1, subsample=1, colsample_bytree=1)
	xgbCV <- xgb.cv(params = params, data = dtrain, nfold = 5, nrounds = 100, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)
	
	print("##################################### DEFAULT PARAMETERS WITH OPTIMAL ITERATIONS ###############################################")
	best_metric = min(xgbCV$test.error.mean)
	best_iteration = which(xgbCV$test.error.mean == min(xgbCV$test.error.mean))
	print(paste("Best Iteration: ",best_iteration,"; ","Best Accuracy: ",1-best_metric,sep = ""))
	print("##################################### HYPERPARAMETER TUNING STARTS #############################################################")
	print("\n")
	print("##################################### TUNING max_depth and min_child_weight ####################################################")

	### max_depth and min_child_weight 
	max_depth = c(3:8)
	min_child_weight = c(1:6)
	parameters1 = data.table()
	for(i in 1:length(max_depth)){
		for(j in 1:length(max_depth)){
			params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=max_depth[i], min_child_weight=min_child_weight[j], subsample=1, colsample_bytree=1)
			xgbCV1 <- xgb.cv(params = params, data = dtrain, nfold = 5, nrounds = 100, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)
			parameters1 = rbind(parameters1,data.table(max_depth = max_depth[i],min_child_weight = min_child_weight[j],Error = min(xgbCV1$test.error.mean)))
		}
	}

	min_err = min(parameters1$Error)
	print("################################### BEST PARAMETERS #############################################################################")
	print(paste("max_depth: ",parameters1[which(Error == min_err),max_depth],";","min_child_weight:",parameters1[which(Error == min_err),min_child_weight],";",sep = ""))
	best_params["max_depth"] = parameters1[which(Error == min_err),max_depth]
	best_params["min_child_weight"] = parameters1[which(Error == min_err),min_child_weight]
	best_params["nrounds"] = which(xgbCV1$test.error.mean == min(xgbCV1$test.error.mean))
	
	### subsample and colsample_bytree
	subsample = seq(0.5,1,by = 0.1)
	colsample_bytree = seq(0.5,1,by = 0.1)
	parameters2 = data.table()
	for(i in 1:length(max_depth)){
		for(j in 1:length(max_depth)){
			params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=best_params[["max_depth"]], min_child_weight=best_params[["min_child_weight"]], subsample=subsample[i], colsample_bytree=colsample_bytree[j])
			xgbCV2 <- xgb.cv(params = params, data = dtrain, nfold = 5, nrounds = best_params[["nrounds"]], showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)
			parameters2 = rbind(parameters2,data.table(subsample = subsample[i],colsample_bytree = colsample_bytree[j],Error = min(xgbCV2$test.error.mean)))
		}
	}

	min_err = min(parameters2$Error)
	print("################################### BEST PARAMETERS #############################################################################")
	print(paste("subsample: ",parameters2[which(Error == min_err),subsample],";","colsample_bytree:",parameters2[which(Error == min_err),colsample_bytree],";",sep = ""))
	best_params["subsample"] = parameters2[which(Error == min_err),subsample]
	best_params["colsample_bytree"] = parameters2[which(Error == min_err),colsample_bytree]
	best_params["nrounds"] = which(xgbCV2$test.error.mean == min(xgbCV2$test.error.mean))
	
	### Tune for gamma
	gamma = c(0,0.5,by = 0.1)
	parameters3 = data.table()
	for(i in 1:length(gamma)){
			params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=gamma[i], max_depth=best_params[["max_depth"]], min_child_weight=best_params[["min_child_weight"]], subsample=best_params[["subsample"]], colsample_bytree=best_params[["colsample_bytree"]])
			xgbCV3 <- xgb.cv(params = params, data = dtrain, nfold = 5, nrounds = best_params[["nrounds"]], showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)
			parameters3 = rbind(parameters3,data.table(gamma = gamma[i],Error = min(xgbCV3$test.error.mean)))
	}
	min_err = min(xgbCV3$test.error.mean)
	best_params["gamma"] = parameters3[which(Error == min_err),gamma]
	best_params["nrounds"] = which(xgbCV3$test.error.mean == min(xgbCV3$test.error.mean))
	print(paste("gamma: ",best_params[["gamma"]]),sep = "")


	### Tune for nrounds for the final set of hyperparameters...
	params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=best_params[["max_depth"]], min_child_weight=best_params[["min_child_weight"]], subsample=best_params[["subsample"]], colsample_bytree=best_params[["colsample_bytree"]])
	xgbCV4 <- xgb.cv(params = params, data = dtrain, nfold = 5, nrounds = 100, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)
	opt_nrounds = which(xgbCV4$test.error.mean == min(xgbCV4$test.error.mean))
	best_params["nrounds"] = opt_nrounds[1]

	### Train the model with the optimal set of parameters...
	xgb <- xgb.train(data = dtrain,
	 max_depth = best_params[["max_depth"]],
	 nrounds =best_params[["nrounds"]], 
	 gamma = best_params[["gamma"]],
	 booster = "gbtree",
	 objective = "binary:logistic",
	 subsample = best_params[["subsample"]],
	 colsample_bytree = best_params[["colsample_bytree"]],
	 watchlist = list(eval=dvalid,train = dtrain),
	 print.every.n = 5
	)
	return(xgb)

}