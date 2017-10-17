rm(list=ls())
setwd("F:\\Mach. Learning\\Kaggle case studies\\ML Challenge")

### Load required libraries
library(data.table)
library(caret)
library(caretEnsemble)
library(stringr)

### Input

dt = fread("train.csv",showProgress=TRUE)


sapply(dt, function(x) sum(is.na(x)) + length(which(x=="")))


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

### Removing rows (3) containing NAs

dt = dt[-which(name==""),]
dt = dt[-which(desc==""),]

### Factorizing...
dt[,final_status := as.factor(as.character(final_status))]
dt[,disable_communication := as.numeric(as.factor(disable_communication))-1]
dt[,currency := as.factor(currency)]
dt[,country := ifelse(country %in% c("DE","DK","IE","NL","NO","NZ","SE"),"Others",country)]

dt[,country := as.factor(country)]

### Combining infrequent levels...

### Converting everything to dollars...
dt[,goal1 := ifelse(currency=="GBP",1.28*goal,ifelse(currency %in% c("CAD","AUD"),0.76*goal,ifelse(currency=="NZD",0.72*goal,ifelse(currency=="EUR",1.12*goal,ifelse(currency=="SEK",0.11*goal,ifelse(currency=="NOK",0.12*goal,0.15*goal))))))]
dt[,goal1 := log(goal1)]
### Removing unique columns...
dt[,project_id := NULL]
text_features = c("name","desc","keywords")
len_features = c("name_len","desc_len","keywords_len")
cnt_features = c("name_cnt","desc_cnt","keywords_cnt")
dt[,c(len_features) := lapply(.SD,function(x) str_count(x)),.SDcols = text_features]
dt[,c(cnt_features) := lapply(.SD,function(x) str_count(x,"\\w+")),.SDcols = text_features]


dt[, c("currency","goal","backers_count","name","desc","keywords") := NULL]
dt[,new_name := NULL]

dt[,goal1 := outlier_cap(goal1)]


dt[,deadlineTS := as.POSIXct(deadline,origin = "1970-01-01")]
dt[,state_changed_atTS := as.POSIXct(state_changed_at,origin = "1970-01-01")]
dt[,created_atTS := as.POSIXct(created_at,origin = "1970-01-01")]
dt[,launched_atTS := as.POSIXct(launched_at,origin = "1970-01-01")]

### Feature Engineering...
dt[,Diff_Deadline_created := as.numeric(round(difftime(deadlineTS,created_atTS,units = "days")))]
dt[,Diff_launched_state_changed := as.numeric(round(difftime(state_changed_atTS,launched_atTS,units = "days")))]
dt[,Diff_created_launched := as.numeric(round(difftime(launched_atTS,created_atTS,units = "days")))]
dt[,Diff_launched_deadline := as.numeric(round(difftime(deadlineTS,launched_atTS,units = "days")))]
dt[,Diff_state_changed_deadline := as.numeric(round(difftime(deadlineTS,state_changed_atTS,units = "days")))]

dt[,createdMonth := month(created_atTS)]
dt[,deadlineMonth := month(deadlineTS)]
dt[,createdMonth := as.factor(createdMonth)]
dt[,deadlineMonth := as.factor(deadlineMonth)]

dt[,deadlineTS := NULL]
dt[,state_changed_atTS := NULL]
dt[,created_atTS := NULL]
dt[,launched_atTS := NULL]
dt[,deadline := NULL]
dt[,state_changed_at := NULL]
dt[,created_at := NULL]
dt[,launched_at := NULL]



dt$new_name = sapply(c(1:nrow(dt)),function(x) get_name(x))
dt[,album_ind := sapply(dt$new_name,function(x) as.numeric("album" %in% unlist(str_split(x," "))))]
dt[,music_ind := sapply(dt$new_name,function(x) as.numeric("music" %in% unlist(str_split(x," "))))]
dt[,game_ind := sapply(dt$new_name,function(x) as.numeric("game" %in% unlist(str_split(x," "))))]
dt[,book_ind := sapply(dt$new_name,function(x) as.numeric("book" %in% unlist(str_split(x," "))))]
dt[,film_ind := sapply(dt$new_name,function(x) as.numeric("film" %in% unlist(str_split(x," "))))]
dt[,project_ind := sapply(dt$new_name,function(x) as.numeric("project" %in% unlist(str_split(x," "))))]
dt[,art_ind := sapply(dt$new_name,function(x) as.numeric("art" %in% unlist(str_split(x," "))))]
dt[,new_ind := sapply(dt$new_name,function(x) as.numeric("new" %in% unlist(str_split(x," "))))]
dt[,debut_ind := sapply(dt$new_name,function(x) as.numeric("debut" %in% unlist(str_split(x," "))))]
dt[,documentari_ind := sapply(dt$new_name,function(x) as.numeric("documentari" %in% unlist(str_split(x," "))))]
dt[,love_ind := sapply(dt$new_name,function(x) as.numeric("love" %in% unlist(str_split(x," "))))]
dt[,adventur_ind := sapply(dt$new_name,function(x) as.numeric("adventur" %in% unlist(str_split(x," "))))]
dt[,record_ind := sapply(dt$new_name,function(x) as.numeric("record" %in% unlist(str_split(x," "))))]
dt[,help_ind := sapply(dt$new_name,function(x) as.numeric("help" %in% unlist(str_split(x," "))))]
dt[,cancel_ind := sapply(dt$new_name,function(x) as.numeric("cancel" %in% unlist(str_split(x," "))))]


set.seed(100)
ind = createDataPartition(dt$final_status,p=0.70,list = FALSE)
train = dt[ind,]
test = dt[-ind,]



trainControl = trainControl(method="cv",number = 5)
params = data.frame(interaction.depth=3,n.trees = c(100,200,300),shrinkage = 0.1,n.minobsinnode = 10)
gb = train(final_status~.,data = train,method = "gbm",trControl = trainControl,tuneGrid = params)

h2o.init(nthreads = -1,max_mem_size = '16G')
trainh2o = as.h2o(train)
testh2o = as.h2o(test)
features = names(train)[!names(train) %in% c("final_status")]
rf <- h2o.randomForest(x=features, validation_frame = testh2o,
                          y="final_status", 
                          ntrees = 1500,
                          max_depth = 10,
                          nfolds = 5,
                          training_frame=trainh2o)

test2 = subset(test1,select=setdiff(colnames(train),"final_status"))

p1 = predict(rf,test,type="prob")
p1 = p1[,2]


test1 = fread("test.csv",verbose = TRUE)
test1[,deadlineTS := as.POSIXct(deadline,origin = "1970-01-01")]
test1[,state_changed_atTS := as.POSIXct(state_changed_at,origin = "1970-01-01")]
test1[,created_atTS := as.POSIXct(created_at,origin = "1970-01-01")]
test1[,launched_atTS := as.POSIXct(launched_at,origin = "1970-01-01")]
test1[,Diff_Deadline_created := as.numeric(round(difftime(deadlineTS,created_atTS,units = "days")))]
test1[,Diff_launched_state_changed := as.numeric(round(difftime(state_changed_atTS,launched_atTS,units = "days")))]
test1[,Diff_created_launched := as.numeric(round(difftime(launched_atTS,created_atTS,units = "days")))]
test1[,Diff_launched_deadline := as.numeric(round(difftime(deadlineTS,launched_atTS,units = "days")))]
test1[,goal1 := ifelse(currency=="GBP",1.28*goal,ifelse(currency %in% c("CAD","AUD"),0.76*goal,ifelse(currency=="NZD",0.72*goal,ifelse(currency=="EUR",1.12*goal,ifelse(currency=="SEK",0.11*goal,ifelse(currency=="NOK",0.12*goal,ifelse(currency=="CHF",1.03*goal,ifelse(currency=="SGD",0.72*goal,ifelse(currency=="MXN",0.055*goal,0.13*goal)))))))))]

test1[,disable_communication := as.factor(disable_communication)]
test1[,currency := as.factor(currency)]

test1[,country := ifelse(country %in% c("AU","CA","GB","US"),country,"Others")]
test1[,country := as.factor(country)]

text_features = c("name","desc","keywords")
len_features = c("name_len","desc_len","keywords_len")
cnt_features = c("name_cnt","desc_cnt","keywords_cnt")
test1[,c(len_features) := lapply(.SD,function(x) str_count(x)),.SDcols = text_features]
test1[,c(cnt_features) := lapply(.SD,function(x) str_count(x,"\\w+")),.SDcols = text_features]

name = Corpus(VectorSource(test1$name))
name = tm_map(name,stripWhitespace)
name = tm_map(name,tolower)
name = tm_map(name,PlainTextDocument)
name = tm_map(name,removePunctuation)
name = tm_map(name,removeWords,stopwords('english'))
name = tm_map(name,stemDocument)

test1$new_name = sapply(c(1:nrow(test1)),function(x) get_name(x))
test1[,album_ind := sapply(test1$new_name,function(x) as.numeric("album" %in% unlist(str_split(x," "))))]
test1[,music_ind := sapply(test1$new_name,function(x) as.numeric("music" %in% unlist(str_split(x," "))))]
test1[,game_ind := sapply(test1$new_name,function(x) as.numeric("game" %in% unlist(str_split(x," "))))]
test1[,book_ind := sapply(test1$new_name,function(x) as.numeric("book" %in% unlist(str_split(x," "))))]
test1[,film_ind := sapply(test1$new_name,function(x) as.numeric("film" %in% unlist(str_split(x," "))))]
test1[,project_ind := sapply(test1$new_name,function(x) as.numeric("project" %in% unlist(str_split(x," "))))]
test1[,art_ind := sapply(test1$new_name,function(x) as.numeric("art" %in% unlist(str_split(x," "))))]
test1[,new_ind := sapply(test1$new_name,function(x) as.numeric("new" %in% unlist(str_split(x," "))))]
test1[,debut_ind := sapply(test1$new_name,function(x) as.numeric("debut" %in% unlist(str_split(x," "))))]
test1[,documentari_ind := sapply(test1$new_name,function(x) as.numeric("documentari" %in% unlist(str_split(x," "))))]
test1[,love_ind := sapply(test1$new_name,function(x) as.numeric("love" %in% unlist(str_split(x," "))))]
test1[,adventur_ind := sapply(test1$new_name,function(x) as.numeric("adventur" %in% unlist(str_split(x," "))))]
test1[,record_ind := sapply(test1$new_name,function(x) as.numeric("record" %in% unlist(str_split(x," "))))]
test1[,help_ind := sapply(test1$new_name,function(x) as.numeric("help" %in% unlist(str_split(x," "))))]
test1[,cancel_ind := sapply(test1$new_name,function(x) as.numeric("cancel" %in% unlist(str_split(x," "))))]

p = predict(models$gbm,test1,type="prob")
test1$final_status = p
test1 = data.frame(test1)

final = test1[,c("project_id","final_status")]
write.csv(final,"Submission7.csv",row.names=F)


dt[,final_status := as.numeric(final_status)-1]
dt[,disable_communication := as.numeric(disable_communication)-1]
dt[,createdMonth := as.numeric(createdMonth)]
dt[,deadlineMonth := as.numeric(deadlineMonth)]
set.seed(100)
ind = sample(1:nrow(dt),round(0.7*nrow(dt)),replace=F)
dt[,final_status := as.numeric(as.character(final_status))]
train = dt[ind,]
test = dt[-ind,]
dtrain = xgb.DMatrix(data = sparse.model.matrix(final_status~.-1,data=train),label=train$final_status)
dtest = xgb.DMatrix(data = sparse.model.matrix(final_status~.-1,data=test),label=test$final_status)

xgb <- xgb.train(data = dtrain, 
	 label = "final_status", 
	 max_depth = 5, 
	 nrounds =200, 
	 lambda = 0.1,
	 objective = "binary:logistic",
	 subsample = 0.75,
	 colsample_bytree = 0.75,
	 watchlist = list(eval=dtest,train = dtrain),
	 print.every.n = 10
	)

p2 = predict(xgb,dtest)
table(p2>0.5,test$final_status)

fit = glm(final_status~.,data = train,family=binomial)


trainControl = trainControl(method="cv",number = 5)
Fit = train(final_status~.,data = train,method = "ranger",trControl = trainControl)

### Word cloud...

library(tm)
df_bkp = df
df = dt
df = df[final_status=="1",]

name = Corpus(VectorSource(dt$name))
name = tm_map(name,stripWhitespace)
name = tm_map(name,tolower)
name = tm_map(name,PlainTextDocument)
name = tm_map(name,removePunctuation)
name = tm_map(name,removeWords,stopwords('english'))
name = tm_map(name,stemDocument)

keywords = Corpus(VectorSource(dt$keywords))
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
keywords = tm_map(keywords,toSpace,"-")
keywords = tm_map(keywords,stripWhitespace)
keywords = tm_map(keywords,tolower)
keywords = tm_map(keywords,PlainTextDocument)
keywords = tm_map(keywords,removePunctuation)
keywords = tm_map(keywords,removeWords,stopwords('english'))
keywords = tm_map(keywords,stemDocument)



library(wordcloud)
wordcloud(name,max.words=100,random.order = FALSE,rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

top_categories = c("album","music","game","book","film","project","art","new","debut","documentari","love","adventur","record","help","cancel")
cat_features = paste(top_categories,"_len",sep="")

dt_bkp = dt
dt[,row_ind := 1:nrow(dt)]

get_name <- function(ind){
	name1 = as.character(name[[ind]])
	return(name1)
}

dt$new_name = sapply(c(1:nrow(dt)),function(x) get_name(x))
dt[,album_ind := sapply(dt$new_name,function(x) as.numeric("album" %in% unlist(str_split(x," "))))]
dt[,music_ind := sapply(dt$new_name,function(x) as.numeric("music" %in% unlist(str_split(x," "))))]
dt[,game_ind := sapply(dt$new_name,function(x) as.numeric("game" %in% unlist(str_split(x," "))))]
dt[,book_ind := sapply(dt$new_name,function(x) as.numeric("book" %in% unlist(str_split(x," "))))]
dt[,film_ind := sapply(dt$new_name,function(x) as.numeric("film" %in% unlist(str_split(x," "))))]
dt[,project_ind := sapply(dt$new_name,function(x) as.numeric("project" %in% unlist(str_split(x," "))))]
dt[,art_ind := sapply(dt$new_name,function(x) as.numeric("art" %in% unlist(str_split(x," "))))]
dt[,new_ind := sapply(dt$new_name,function(x) as.numeric("new" %in% unlist(str_split(x," "))))]
dt[,debut_ind := sapply(dt$new_name,function(x) as.numeric("debut" %in% unlist(str_split(x," "))))]
dt[,documentari_ind := sapply(dt$new_name,function(x) as.numeric("documentari" %in% unlist(str_split(x," "))))]
dt[,love_ind := sapply(dt$new_name,function(x) as.numeric("love" %in% unlist(str_split(x," "))))]
dt[,adventur_ind := sapply(dt$new_name,function(x) as.numeric("adventur" %in% unlist(str_split(x," "))))]
dt[,record_ind := sapply(dt$new_name,function(x) as.numeric("record" %in% unlist(str_split(x," "))))]
dt[,help_ind := sapply(dt$new_name,function(x) as.numeric("help" %in% unlist(str_split(x," "))))]
dt[,cancel_ind := sapply(dt$new_name,function(x) as.numeric("cancel" %in% unlist(str_split(x," "))))]
#### Ensembling

set.seed(100)
dt_bkp = dt
dt[,final_status := ifelse(final_status=="1","Success","Failure")]
dt[,final_status := as.factor(final_status)]
ind = createDataPartition(y = dt$final_status , p = 0.70, list = FALSE)
train = dt[ind,]
test = dt[-ind,]



train[,final_status := ifelse(final_status=="1","Success","Failure")]
train[,final_status := as.factor(final_status)]
test[,final_status := ifelse(final_status=="1","Success","Failure")]
test[,final_status := as.factor(final_status)]

library(caretEnsemble)
algorithms <- c("glm","rpart","gbm")
trainControl <- trainControl(method = "cv", number = 5,savePredictions = "final",classProbs = TRUE,verbose = FALSE)
models <- caretList(final_status~.,data = train,methodList = algorithms,trControl = trainControl)

greedy_ensemble <- caretEnsemble(
  models, 
  metric="Accuracy",
  trControl=trainControl(
    number=2,
    classProbs=TRUE
    ))
summary(greedy_ensemble)

p = predict(gbm_ensemble,test1,type="prob")
test1$final_status = ifelse(p>=0.5,1,0)
write.csv(test1[,.(project_id,final_status)],"Submission8.csv",row.names=F)

gbm_ensemble <- caretStack(models,method = "gbm",metric="Accuracy",
  trControl=trainControl(
    number=2,
    classProbs=TRUE,
    verbose = FALSE
    ))
summary(gbm_ensemble)


algorithms <- c("rpart","glm")#"gbm","knn","ranger","nnet","ada")

best_ensemble <- function(algorithms,train){
	c = 1
	model_list = list()
	trainControl <- trainControl(method = "cv", number = 5,savePredictions = "final",classProbs = TRUE)
	total_algo = length(algorithms)
	for(i in 2:total_algo-1){
		com_mat = combn(algorithms,m=i)
		for(k in 1:ncol(com_mat)){
			alg = com_mat[,k]
			models <- caretList(final_status~.,data = train,methodList = alg,trControl = trainControl)
			greedy_ensemble <- caretEnsemble(
			  models, 
			  metric="Accuracy",
			  trControl=trainControl(
			    number=5,
			    classProbs=TRUE
			    ))
			model_list[[c]] = greedy_ensemble
			c = c+1
		}
	}
	return(model_list)
}

accuracy<-function(actual,prediction){
	return(sum(actual==prediction)/length(actual))
}


get_cutoff <- function(model,test){
	test[,final_status := ifelse(final_status=="Success",1,0)]
	prediction = predict(model,test,type="prob")
	final = data.frame()
	threshold = seq(0,1,by = 0.01)
	for(i in threshold){
		class = ifelse(prediction>i,1,0)
		acc = accuracy(test$final_status,class)
		final = rbind(final,data.frame(threshold = i,Accuracy = acc))
	}
	return(final)
}	

get_cutoff1 <- function(model,test){
	prediction = predict(model,test,type="prob")
	prediction = prediction[,2]
	final = data.frame()
	threshold = seq(0,1,by = 0.01)
	for(i in threshold){
		class = ifelse(prediction>i,1,0)
		acc = accuracy(test$final_status,class)
		final = rbind(final,data.frame(threshold = i,Accuracy = acc))
	}
	return(final)
}	