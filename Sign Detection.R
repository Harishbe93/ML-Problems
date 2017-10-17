rm(list=ls())
gc()
setwd("F:\\Mach. Learning\\Kaggle case studies\\India Hacks\\Problem 1 - Traffic Sign Detection")

library(data.table)
library(caret)
library(ggplot2)

df = fread("train.csv",showProgress=F)

df[,DetectedCamera := as.factor(DetectedCamera)]
names(df)[ncol(df)]="Target"
df[,Target := as.factor(Target)]
### EDA...

sapply(df,function(x) sum(is.na(x))+length(which(x=="")))   ### No missing values in the dataset...

df[,Perc := c(1:nrow(df))/nrow(df)]
		
### Outliers cappping...

outlier_cap <- function(x){
	a = quantile(x,.25)
	b = quantile(x,.75)
	IQR = b - a
	LB = a - (1.5*IQR)
	UP = b + (1.5*IQR)
	x[x>UP] = UP
	x[x<LB] = LB
	return(x)
}

oversample <- function(data,class,num){
	sub = data[Target==class]
	ind = sample(1:nrow(sub),num,replace=T)
	return(sub[ind,])
}

left = oversample(df,"Left",7000)
right = oversample(df,"Right",7000)
df = rbind(df,left,right)

dt = df
df = data.frame(df)
ggplot(df,aes(DetectedCamera,AngleOfSign))+geom_boxplot()
df[df$DetectedCamera=="Front","AngleOfSign"] = outlier_cap(df[df$DetectedCamera=="Front","AngleOfSign"])
df[df$DetectedCamera=="Rear","AngleOfSign"] = outlier_cap(df[df$DetectedCamera=="Rear","AngleOfSign"])
df[df$DetectedCamera=="Left","AngleOfSign"] = outlier_cap(df[df$DetectedCamera=="Left","AngleOfSign"])
df[df$DetectedCamera=="Right","AngleOfSign"] = outlier_cap(df[df$DetectedCamera=="Right","AngleOfSign"])
ggplot(df,aes(DetectedCamera,AngleOfSign))+geom_boxplot()


## Iter 2

df[,SignWidth := outlier_cap(SignWidth)]
df[,SignHeight := outlier_cap(SignHeight)]
df[,SignAspectRatio := outlier_cap(SignAspectRatio)]


df$FrontFlag1 = ifelse(df$AngleOfSign>300 & df$SignAspectRatio<1,1,0)
df$FrontFlag2 = ifelse(df$DetectedCamera=="Right" & df$SignWidth>60 & df$AngleOfSign %in% c(0:80),1,0)
df$FrontFlag3 = ifelse(df$DetectedCamera=="Left" & df$AngleOfSign %in% c(270:360) & df$SignWidth>50,1,0)

df$RearFlag1 = ifelse(df$DetectedCamera=="Left" & df$SignWidth>70 & df$AngleOfSign %in% c(170:260),1,0)
df$RearFlag2 = ifelse(df$DetectedCamera=="Left" & df$AngleOfSign %in% c(170:260) & df$SignAspectRatio<0.5,1,0)
df$RearFlag3 = ifelse(df$DetectedCamera=="Left" & df$AngleOfSign %in% c(170:260) & df$SignAspectRatio<1.1,1,0)



df$FrontFlag2 = ifelse(df$AngleOfSign %in% c(0:85),1,0)
df$FrontFlag3 = ifelse(df$SignAspectRatio<0.46 & df$AngleOfSign %in% c(0:85,320:360),1,0)
df$FrontFlag4 = ifelse(df$DetectedCamera=="Right" & df$SignWidth>50,1,0)

df$LeftFlag1 = ifelse(df$SignHeight<50 & df$SignWidth<50 & df$AngleOfSign %in% c(260:350) & df$DetectedCamera=="Left",1,0)


df$RightFlag1 = ifelse(df$SignHeight<50 & df$SignWidth<50 & df$AngleOfSign %in% c(90:150) & df$DetectedCamera=="Right",1,0)

df$RearFlag1 = ifelse(df$AngleOfSign %in% c(190:250),1,0)

df$RearFlag2 = ifelse(df$AngleOfSign %in% c(100:180) & df$DetectedCamera=="Right" & df$SignWidth>50,1,0)
df$RearFlag3 = ifelse(df$DetectedCamera=="Left" & df$SignWidth>50,1,0)

df = data.table(df)
df[,Id := NULL]
df$AngleType = ifelse(df$AngleOfSign<=90,"<90",ifelse(df$AngleOfSign>90 & df$AngleOfSign<=180,"90 - 180",ifelse(df$AngleOfSign>180 & df$AngleOfSign<=270,"180 - 270","270 - 360")))
df[,AngleType:=as.factor(df$AngleType)]

df$AngleType2 = ifelse(df$AngleOfSign>315 & df$AngleOfSign<=45,"315 - 45",ifelse(df$AngleOfSign>45 & df$AngleOfSign<=135,"45 - 135",ifelse(df$AngleOfSign>135 & df$AngleOfSign<=225,"135- 225","225 - 315")))
df[,AngleType2:=as.factor(df$AngleType2)]


df$AspectRatioShrunk = ifelse(df$SignAspectRatio<0.5,"Large",ifelse(df$SignAspectRatio>0.5 & df$SignAspectRatio<=1,"Medium","Small"))
df$Area = df$SignWidth * df$SignHeight
df$AbsDifference = abs(df$SignHeight - df$SignWidth)
#df$MoreHeightLessWidth = ifelse(df$AspectRatioShrunk=="Large" & df$SignHeight>df$SignWidth,1,0)
df$FrontFlag = ifelse(df$DetectedCamera=="Right" & df$AngleOfSign %in% c(0:75),1,0)
df$RearFlag = ifelse(df$DetectedCamera=="Left" & df$AngleOfSign %in% c(220:300),1,0)

df$FrontFlag1 = ifelse(df$Area>21840,1,0)

set.seed(100)
ind = createDataPartition(df$Target,p=0.70,list=FALSE)
train = df[ind,]
test = df[-ind,]

trainControl = trainControl(method="cv",number = 5)
algorithms <- c("knn","rpart","gbm","nnet")
models <- caretList(Target~.,data = train,methodList = algorithms,trControl = trainControl)

params = data.frame(k=seq(5,15,by=3))
gbFit = train(Target~.,data = train,method ="gbm",trControl = trainControl)


params = data.frame(mtry = seq(5,11,by=2))
RfFit = train(Target~.,data=train,method="rf",trControl = trainControl, tuneGrid = params) 
KnnFit = train(Target~.,data=train,method="knn",trControl=trainControl)
GBTFit = train(Target~.,data=train,method = "nb",trControl = trainControl,tuneLength = 5)
NBFit = train(Target~.,data=train,method = "lda",trControl = trainControl,tuneLength = 3)
# DTFit = train(Target~.,data = train,method = "rpart",trControl = trainControl,tuneLength=3)


train_index <- sample(1:nrow(df), nrow(df)*0.75)
df = data.frame(df)
# Full data set
data_variables <- as.matrix(df[,-ncol(df)])
data_label <- df[,"Target"]

df$Target = ifelse(df$Target=="Front",0,ifelse(df$Target=="Rear",1,ifelse(df$Target=="Left",2,3)))
set.seed(100)
ind = sample(1:nrow(df),round(0.7*nrow(df)),replace=F)
train = df[ind,]
test = df[-ind,]
dtrain = xgb.DMatrix(data = sparse.model.matrix(Target~.-1,data=train),label=train$Target)
dtest = xgb.DMatrix(data = sparse.model.matrix(Target~.-1,data=test),label=test$Target)

data_matrix <- xgb.DMatrix(data = as.matrix(df), label = data_label)
# split train data and make xgb.DMatrix
train_data   <- data_variables[train_index,]
train_label  <- data_label[train_index]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
# split test data and make xgb.DMatrix
test_data  <- data_variables[-train_index,]
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)


xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = 4)
nround    <- 100
cv.nfold  <- 5
cv_model <- xgb.cv(params = xgb_params,
                   data = dtrain, 
                   watchlist = list(val=dtest,train=dtrain),
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = FALSE,
                   stratified = T,
                   showsd = T,
                   print.every.n = 10,
                   prediction = TRUE)


	xgb <- xgb.train(data = dtrain, 
		 label = "Target", 
		 max_depth = 6, 
		 nrounds =250, 
		 objective = "multi:softprob",
		 num_class = 4,
		 subsample = 0.75,
		 colsample_bytree = 0.75,
		 eval_metric = "mlogloss",
		 watchlist = list(eval=dtest,train = dtrain),
		 print.every.n = 20
		)

p = predict(xgb,dtest1)
m = matrix(p,nrow=nrow(test1),ncol=4,byrow=T)
m = data.frame(m)
names(m)=c("Front","Rear","Left","Right")
x1 = cbind(Id = p1,m)

p = predict(knnFit,test1)
table(p,test$Target)


p = predict(ada,test)
table(p,test$Target)

test1 = fread("test.csv")

test1[,SignWidth := outlier_cap(SignWidth)]
test1[,SignHeight := outlier_cap(SignHeight)]
test1[,SignAspectRatio := outlier_cap(SignAspectRatio)]


test1$AngleType = ifelse(test1$AngleOfSign<=90,"<90",ifelse(test1$AngleOfSign>90 & test1$AngleOfSign<=180,"90 - 180",ifelse(test1$AngleOfSign>180 & test1$AngleOfSign<=270,"180 - 270","270 - 360")))
test1[,AngleType:=as.factor(test1$AngleType)]
test1$Area = test1$SignWidth * test1$SignHeight
test1$AspectRatioShrunk = ifelse(test1$SignAspectRatio<0.5,"Large",ifelse(test1$SignAspectRatio>0.5 & test1$SignAspectRatio<=1,"Medium","Small"))
test1$AbsDifference = abs(test1$SignHeight - test1$SignWidth)
test1$MoreHeightLessWidth = ifelse(test1$AspectRatioShrunk=="Large" & test1$SignHeight>test1$SignWidth,1,0)

test1$FrontFlag = ifelse(test1$DetectedCamera=="Right" & test1$AngleOfSign %in% c(0:75),1,0)
test1$RearFlag = ifelse(test1$DetectedCamera=="Left" & test1$AngleOfSign %in% c(220:300),1,0)

test1$AreaFlag = ifelse(test1$Area>6346,1,0)
	p1=test1$Id
	test1$Id = NULL
	dtest1 = xgb.DMatrix(data = sparse.model.matrix(~.-1,data=test1))

p = predict(xgb,dtest1)
m = matrix(p,nrow=nrow(test1),ncol=4,byrow=T)
m = data.frame(m)
names(m)=c("Front","Rear","Left","Right")
x1 = cbind(Id = p1,m)




test1$FrontFlag1 = ifelse(test1$AngleOfSign>300 & test1$SignAspectRatio<1,1,0)
test1$FrontFlag2 = ifelse(test1$DetectedCamera=="Right" & test1$SignWidth>60 & test1$AngleOfSign %in% c(0:80),1,0)
test1$FrontFlag3 = ifelse(test1$DetectedCamera=="Left" & test1$AngleOfSign %in% c(270:360) & test1$SignWidth>50,1,0)

test1$RearFlag1 = ifelse(test1$DetectedCamera=="Left" & test1$SignWidth>70 & test1$AngleOfSign %in% c(170:260),1,0)
test1$RearFlag2 = ifelse(test1$DetectedCamera=="Left" & test1$AngleOfSign %in% c(170:260) & test1$SignAspectRatio<0.5,1,0)
test1$RearFlag3 = ifelse(test1$DetectedCamera=="Left" & test1$AngleOfSign %in% c(170:260) & test1$SignAspectRatio<1.1,1,0)


test1$FrontFlag1 = ifelse(test1$AngleOfSign>300 & test1$SignAspectRatio<1,1,0)
test1$FrontFlag2 = ifelse(test1$DetectedCamera=="Right" & test1$SignWidth>60 & test1$AngleOfSign %in% c(0:80),1,0)
test1$FrontFlag3 = ifelse(test1$DetectedCamera=="Left" & test1$AngleOfSign %in% c(270:360) & test1$SignWidth>50,1,0)

test1$RearFlag1 = ifelse(test1$DetectedCamera=="Left" & test1$SignWidth>70 & test1$AngleOfSign %in% c(170:260),1,0)
test1$RearFlag2 = ifelse(test1$DetectedCamera=="Left" & test1$AngleOfSign %in% c(170:260) & test1$SignAspectRatio<0.5,1,0)
test1$RearFlag3 = ifelse(test1$DetectedCamera=="Left" & test1$AngleOfSign %in% c(170:260) & test1$SignAspectRatio<1.1,1,0)



test1$LeftFlag1 = ifelse(test1$SignHeight<50 & test1$SignWidth<50 & test1$AngleOfSign %in% c(260:350) & test1$DetectedCamera=="Left",1,0)


test1[,DetectedCamera:=as.factor(DetectedCamera)]
p = predict(rf,test1,type="prob")
test1 = data.frame(test1)
p1 = cbind(test1["Id"],p)
r1 = p



test1[,DetectedCamera:=as.factor(DetectedCamera)]
p = predict(rf,test1,type="prob")
p = data.frame(p)
test1 = data.frame(test1)
r1 = cbind(Id=p1,p)
r2 = p


n1 = predict(NBFit,test1,type="raw")
n1 = cbind(test1["Id"],n1)

x1 = x1[,c(1,2,4,3,5)]

r=apply(r1[,2:5],c(1,2),function(x) x*0.2)
r = cbind(r1["Id"],r)
x=apply(x1[,2:5],c(1,2),function(x) x*0.2)
x = cbind(x1["Id"],x)

g=apply(g1[,2:5],c(1,2),function(x) x*0.8)
g = cbind(g1["Id"],g)

k=apply(k1[,2:5],c(1,2),function(x) x*0.1)
k = cbind(k1["Id"],k)

d=apply(d1[,2:5],c(1,2),function(x) x*0.4)
d = cbind(d1["Id"],d)

n=apply(n1[,2:5],c(1,2),function(x) x*0.05)
n = cbind(n1["Id"],n)

f = rbind(r,g)

f = rbind(d,g)

	f = data.table(f)
	final = f[,.(Front = sum(Front),Left = sum(Left), Rear = sum(Rear), Right = sum(Right)),.(Id)]

write.csv(final,"Submission68.csv",row.names=F)



GBTFit = train(Target~.,data = train,method = "gbm",trControl = trainControl,tuneLength=4)
g1 = predict(GBT,test1,type="prob")
g1 = cbind(Id=p1,g1)

k1 = predict(knnFit,test1,type = "prob")
k1 = cbind(Id=p1,k1)

DTFit = train(Target~.,data = train,method = "rpart",trControl = trainControl,tuneLength=15)
d1 = predict(DTFit,test1,type = "prob")
d1 = cbind(Id=p1,d1)


SvmFit = train(Target~.,data = train,method = "nnet",trControl = trainControl,tuneLength=3)
n1 = predict(SvmFit,test1,type = "prob")
n1 = cbind(Id=p1,n1)

r1 = data.frame(predict(rf,test1,type="prob"))
r1$Id = p1
r1 = cbind(Id=p1,r1)
f = rbind(x,g)
f = data.table(f)
final = f[,.(Front = mean(Front),Left = mean(Left), Rear = mean(Rear), Right = mean(Right)),.(Id)]

write.csv(g1,"Submission66.csv",row.names=F)


m = model.matrix(~Target+0, data = test)
m = data.frame(m)
names(m) = c("Front","Left","Rear","Right")


m1 = as.matrix(m)
m2 = as.matrix(p)
mm = m1 * m2
mm = data.frame(mm)
mm1 = apply(mm, c(1,2), function(x) min(max(x, 1E-15), 1-1E-15)) 

apply(mm1[1:10,],1,function(x) sum(-log(x)))

sum(-log(mm$new))

train=data.frame(train)
c50_fit = C5.0(train[,-6],train[,"Target"], trials = 100,rules = FALSE
				,control = C5.0Control(CF = 0.05, 
					fuzzyThreshold = TRUE, 
					earlyStopping = TRUE,
					winnow = TRUE))


### Ensemble

algorithms <- c("rf","gbm","knn","nnet","svmRadial")
trainControl <- trainControl(method = "cv", number = 5)

set.seed(100)
models <- caretList(Target~.,data = train,methodList = algorithms)


xgb_tune = function(X, Y, X_test = NULL, Y_test = NULL, hyper_params = NULL, extra_params = NULL, verbose = TRUE, multi_class = FALSE, regression = FALSE, eval_metric = 'logloss', nfold = 5) {
	required_packages = c('Matrix', 'xgboost', 'caret', 'MLmetrics', 'InformationValue')
	if(any(required_packages %in% rownames(installed.packages())) == FALSE) {
		stop('> To run this we need the following packages: \n', paste(required_packages, collapse = '\n'))
	}
	library(Matrix)
	library(xgboost)
	library(caret)
	library(MLmetrics)
	library(InformationValue)

	objective = 'binary:logistic'

	# # convert Y to numeric # #
	Y = as.numeric(as.factor(Y)) - 1

	# # data split # #
	if(is.null(Y_test) | is.null(X_test)) {
		if(regression) {
				eval_metric = 'rmse'
				objective = 'reg:linear'
				index = 1:length(Y)
				index = sample(index, round(length(Y) * 0.75))
			} else {
				index = createDataPartition(Y, p = 0.75, list = FALSE)
			}
	
		X_train = X[index,]
		Y_train = Y[index]
		X_test = X[-index,]
		Y_test = Y[-index]
	} else {
		X_train = X
		Y_train = Y
		Y_test = as.numeric(as.factor(Y_test)) - 1
	}
	# # data preparation - one hot encoding # #
	train_matrix = sparse.model.matrix( ~. -1, data = X_train)
	test_matrix = sparse.model.matrix( ~. -1, data = X_test)

	# # form xgb DMatrix # #
	dtrain = xgb.DMatrix(data = as.matrix(train_matrix), label = Y_train)
	dtest = xgb.DMatrix(data = as.matrix(test_matrix), label = Y_test)

	# # form watch list # #
	watchlist = list(train = dtrain, eval = dtest)

	if(multi_class) {
		eval_metric = "mlogloss"
		objective = "multi:softprob"
		num_class = length(unique(Y))
	}

	if(is.null(hyper_params)) {
		hyper_params = expand.grid(nrounds = c(50, 100, 200),
							eta = c(0.1, 0.2, 0.3),
							max_depth = c(2, 3, 4),
							min_child_weight = c(4, 5),
							colsample_bylevel = c(0.7, 0.8, 0.9),
							subsample = c(0.5, 0.6, 0.7, 1),
							colsample_bytree = c(0.5, 0.75, 1),
							scale_pos_weight = c(1, 2, 5))
	}
	if(is.null(extra_params)) {
		extra_params = list(verbose = FALSE,
			objective = objective,
			eval_metric = eval_metric,
			showsd = TRUE
		)
	}
	if(multi_class) {
		extra_params = c(extra_params, list(num_class = num_class))
	}
	grid_metric = apply(hyper_params, 1, function(eachRow) {
		params = c(as.list(eachRow), extra_params)
		if('nrounds' %in% names(params)) {
			nrounds = params[['nrounds']]
		} else {
			nrounds = 100
		}
		if(verbose) {
			print('> Running for:')
			verbose_print = params
			verbose_print$watchlist = NULL
			print(data.table(do.call('cbind', verbose_print)))
		}
		xgb_fit = xgb.train(data = dtrain, params = params, nfold = nfold, nrounds = nrounds, print.every.n = 10, watchlist = watchlist)
		predicted = predict(xgb_fit, dtest)
		if(regression) {
			return(sqrt(sum((Y_test - predicted) ** 2)/length(predicted)))
		} else if(multi_class) {
			predicted = predict(xgb_fit, dtest)
			predicted = data.table(matrix(predicted, ncol = num_class, byrow = T))
			return(MultiLogLoss(predicted, Y_test))
		} else {
			predicted = predict(xgb_fit, dtest)
			return(performance_measure(predicted = predicted, actual = Y_test, metric = 'fscore', beta = 5))
		}
	})

	l = xgb_tune(X=df[,!names(df) %in% c("Target")],Y=df[,"Target"],hyper_params = hyper_params,multi_class = TRUE)
	res = cbind(l$params,metric = l$metric)
	row_ind = which(res[ncol(res)]==min(res[ncol(res)]))
	xgb <- xgb.train(data = dtrain, 
		 label = "Target", 
		 max_depth = res[row_ind,"max_depth"], 
		 nrounds =res[row_ind,"nrounds"], 
		 objective = "multi:softprob",
		 num_class = 4,
		 subsample = res[row_ind,"subsample"],
		 colsample_bytree = res[row_ind,"colsample_bytree"],
		 eval_metric = "mlogloss",
		 watchlist = list(eval=dtest,train = dtrain),
		 print.every.n = 20
		)

	return(list(params = hyper_params, metric = grid_metric,best_mod = xgb))

}
df = data.frame(df)
hyper_params = expand.grid(nrounds = c(150,200),max_depth = c(4,5,6),sub_sample = c(0.5,0.75,1),colsample_bytree = c(0.5,0.75,1))
l = xgb_tune(X=df[,!names(df) %in% c("Target")],Y=df[,"Target"],hyper_params = hyper_params,multi_class = TRUE)
res = cbind(l$params,metric = l$metric)
which(res[ncol(res)]==min(res[ncol(res)]))


### H2O

library(h2o)
h2o.init(
  nthreads=-1,        
  max_mem_size = "16G")
h2o.removeAll()

rf1 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = train_h2o,        ## the H2O frame for training     ## the H2O frame for validation (not required)
  x=1:5,                        ## the predictor columns, by column index
  y=6,                          ## the target index (what we are predicting)
  model_id = "rf_covType_v1",    ## name the model in H2O
                                 ##   not required, but helps use Flow
  ntrees = 1000,                  ## use a maximum of 200 trees to create the
                                 ##  random forest model. The default is 50.
                                ##  I have increased it because I will let 
                               ##  the early stopping criteria decide when
                                 ##  the random forest is sufficiently accurate
  stopping_rounds = 2,           ## Stop fitting new trees when the 2-tree
                                 ##  average is within 0.001 (default) of 
                                 ##  the prior two 2-tree averages.
                                 ##  Can be thought of as a convergence setting
  score_each_iteration = T,      ## Predict against training and validation for
                                 ##  each tree. Default will skip several.
  seed = 1000000)       