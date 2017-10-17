### Clearing workspace...
rm(list = ls())
gc()
setwd("F:\\Mach. Learning\\Kaggle case studies\\Predict the Happiness")


## Libraries
library(data.table)
library(ggplot2)
library(caret)
library(Matrix)
library(xgboost)
library(stringr)
library(tm)
library(randomForest)

### Read the input files...
dt = fread("train.csv",verbose = T)
test = fread("test.csv")
pos = read.table("positive.txt")
neg = read.table("negative.txt")
### No missing values present..
dt[,User_ID := NULL]
dt[,Is_Response := ifelse(Is_Response == "not happy",1,0)]
dt[,Is_Response := as.factor(Is_Response)]

pos_perc <- function(text){
	split = unlist(strsplit(text," "))
	positives = sum(split %in% pos)
	return(positives)
}

neg_perc <- function(text){
	split = unlist(strsplit(text," "))
	negatives = sum(split %in% neg)
	return(negatives)
}


preprocess <- function(data){
	### Combining the same levels with different names...
	data[,Browser_Used := ifelse(Browser_Used %in% c("Chrome"),"Google Chrome",
	                              ifelse(Browser_Used %in% c("IE","InternetExplorer"),"Internet Explorer",
	                                    ifelse(Browser_Used %in% c("Firefox","Mozilla"),"Mozilla Firefox",Browser_Used)))]
	data[,Browser_Used := as.factor(Browser_Used)]
	data[,Device_Used := as.factor(Device_Used)]
	
	### Feature Engineering...
	data[,WordCount := str_count(Description,"\\S+")]
	data[,NoOfPunc := str_count(Description,"[[:punct:]]")]

	### 1.Sentiment related features...
	
	pos = as.character(pos$V1)

	neg = as.character(neg$V1)
	
	data[,Positive := sapply(Description,pos_perc)]
	data[,Negative := sapply(Description,neg_perc)]
	data[,NegGrtThanPositive := ifelse(Negative>Positive,1,0)]
	return(data)
}

dt = preprocess(dt)

### Modeling....


rf = randomForest(Is_Response~Browser_Used + Device_Used + WordCount + Positive+Negative + NegGrtThanPositive + NoOfPunc,data = dt)


test = preprocess(test)
pred = predict(rf,test,type = "prob")
test[,Is_Response := ifelse(pred[,2]>0.5,"not happy","happy")]
write.csv(test[,.(User_ID,Is_Response)],"Submission10.csv",row.names = F)
### Processing Description...
dt$Description = str_replace_all(dt$Description, "[^[:alnum:]]", " ") ### There were some special characters apart from punctuations..so removing them
docs = Corpus(VectorSource(dt$Description))
docs = tm_map(docs,removePunctuation)
docs = tm_map(docs,content_transformer(tolower))
docs = tm_map(docs,removeNumbers)
docs = tm_map(docs,removeWords,stopwords("english"))
docs = tm_map(docs,stripWhitespace)
docs = tm_map(docs,stemDocument)
dtm = DocumentTermMatrix(docs)

dt = removeSparseTerms(dtm,0.6)

f = findFreqTerms(dtm,lowfreq = 10000)
m = as.matrix(dtm)


freq = colSums(m)
top100 = freq[order(-freq)][1:100]

subset = as.data.table(m[,names(top100)])
data = cbind(dt,subset)
data[,User_ID := NULL]
data[,Description := NULL]
data[,Is_Response := as.factor(Is_Response)]


freq = freq[order(-freq)]
dt = data.table(freq,name = names(freq))
ggplot(dt[1:20],aes(name,freq))+geom_bar(stat="identity")
ggplot(dt,aes(freq))+geom_histogram()+scale_x_log10()
dt1 = dt[freq > 3]
train = as.data.table(as.matrix(dtm))

fit = glm(Is_Response ~ ., data = dt, family = binomial)


### Modeling


ind = createDataPartition(data$Is_Response, p = 0.70, list = F)
train = data[ind,]
valid = data[-ind,]

nb = naiveBayes(Is_Response~.,data = train, laplace = T)
p = predict(nb,valid)

trControl = trainControl(method = "cv", number = 5)
DTfit = train(Is_Response~.,data = train, method = "nb",trControl = trControl)
library(stringr)
test$Description = str_replace_all(test$Description, "[^[:alnum:]]", " ")
test[,WordCount := str_count(Description,"\\S+")]
test[,Positive := sapply(Description,pos_perc)]
test[,Negative := sapply(Description,neg_perc)]
test[,NegGrtThanPositive := ifelse(Negative>Positive,1,0)]

### Processing test data...



docs = Corpus(VectorSource(test$Description))
docs = tm_map(docs,removePunctuation)
docs = tm_map(docs,content_transformer(tolower))
docs = tm_map(docs,removeNumbers)
docs = tm_map(docs,removeWords,stopwords("english"))
docs = tm_map(docs,stripWhitespace)
docs = tm_map(docs,stemDocument)
dtm = DocumentTermMatrix(docs)
m1 = as.matrix(dtm)
mt = as.data.table(m1[,names(top100)])


test[,Browser_Used := ifelse(Browser_Used %in% c("Chrome"),"Google Chrome",
                              ifelse(Browser_Used %in% c("IE","InternetExplorer"),"Internet Explorer",
                                    ifelse(Browser_Used %in% c("Firefox","Mozilla"),"Mozilla Firefox",Browser_Used)))]
test[,Browser_Used := as.factor(Browser_Used)]
test[,Device_Used := as.factor(Device_Used)]
test = cbind(test,mt)
test[,WordCount := str_count(Description,"\\S+")]

p = predict(nb, test)
test$Is_Response = p
test[,Is_Response := ifelse(Is_Response == 1,"not happy","happy")]
prop.table(table(train$Is_Response))

write.csv(test[,c("User_ID","Is_Response"),with = F],"Submission6.csv",row.names = F)



train_clean <- cleanData(dt)
test_clean <- cleanData(test)


library(caret)
library(Matrix)
library(xgboost)

ind = createDataPartition(dt$Is_Response,p = 0.70, list = F)
train = dt[ind,]
valid = dt[-ind,]

train[,Is_Response := as.integer(Is_Response)-1]
valid[,Is_Response := as.integer(Is_Response)-1]
train[,Description := NULL]
valid[,Description := NULL]
dtrain = xgb.DMatrix(data = sparse.model.matrix(Is_Response~.-1,data=train),label=train$Is_Response)
dvalid = xgb.DMatrix(data = sparse.model.matrix(Is_Response~.-1,data=valid),label=valid$Is_Response)

xgb <- xgb.train(data = dtrain,
                 nrounds =100, 
                 max_depth = 3,
                 objective = "binary:logistic",
                 lambda = 5,
                 subsample = 1,
                 colsample_bytree = 1,
                 watchlist = list(eval=dvalid,train = dtrain),
                 print.every.n = 10
)
p = predict(xgb,dvalid)

pred = predict(rf,valid,type = "prob")


fit = glm(Is_Response~.,data = train,family = binomial)