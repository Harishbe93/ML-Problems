rm(list=ls())
setwd("F:/Mach. Learning/Kaggle case studies/Titanic/Kaggle_Titanic-master/")

train=read.csv("train.csv",na.strings=c("NA",""))
test=read.csv("test.csv",na.strings=c("NA",""))
data=rbind(train[,!names(train) %in% c("Survived")],test)
### Plot the missing data in the dataset
# library(Amelia)
# missmap(train,main="Missing values in each variable",col=c("yellow","black"),legend=F)

# print(str(train))

# library(dplyr)
# train=tbl_df(train)
# test=tbl_df(test)
# train$Survived=as.factor(train$Survived)
# train$Pclass=as.factor(train$Pclass)
# train$Name=as.character(train$Name)

# library(ggplot2)
# ggplot(train,aes(x=Sex,fill=Survived))+geom_bar()

# train$title=gsub('(.*,)|(\\..*)',"",train$Name)
# train$title=gsub('^\\s+|\\s+$',"",train$title)

# rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
#                 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
# train$title[train$title == 'Mlle']        <- 'Miss' 
# train$title[train$title == 'Ms']          <- 'Miss'
# train$title[train$title == 'Mme']         <- 'Mrs' 
# train$title[train$title %in% rare_title]  <- 'Rare Title'

# train$Surname=sapply(train$Name,function(x) strsplit(x,split="[,.]")[[1]][1])

# train$Fsize=train$Sibsp+train$Parch+1


sapply(data,function(x) sum(is.na(x)))

### Without Feature engineering ... A very basic approach...

data$PassengerId = data$Ticket = data$Name = NULL

data$Cabin = NULL
data[is.na(data$Age),"Age"] = median(data$Age,na.rm=T)

table(data$Embarked) ### Mode is 'S'
data[is.na(data$Embarked),"Embarked"] = 'S'
data[is.na(data$Fare),"Fare"] = median(data$Fare,na.rm=T)

sapply(data,function(x) sum(is.na(x)))


### Modeling... Max acc obtained is 82.9 and the model is GradientBoostedTree


train$Survived = as.factor(train$Survived)
library(caret)
trainControl = trainControl(method = 'cv',number = 10)


rfFit = train(train[,!names(train) %in% "Survived"],train[,"Survived"],method = 'rf')
DTFit = train(Survived~.,data = train,method = 'rpart')
GBTFit = train(Survived~.,data = train, method = 'gbm',trControl = trainControl,verbose = F)  

### Some feature engineering...

train1=read.csv("train.csv",na.strings=c("NA",""))
test1=read.csv("test.csv",na.strings=c("NA",""))

data = rbind(train1[,!names(train1) %in% "Survived"],test1)


data$title=gsub('(.*,)|(\\..*)',"",data$Name)
data$title=gsub('^\\s+|\\s+$',"",data$title)

data$PassengerId = data$Ticket = data$Name = data$Cabin = NULL
### Combining infrequent levels into "Rare"
freq = names(sort(table(data$title),decreasing=T)[1:6])
data$title = ifelse(data$title %in% freq,data$title,"Rare")

data[is.na(data$Age),"Age"] = median(data$Age,na.rm=T)

table(data$Embarked) ### Mode is 'S'
data[is.na(data$Embarked),"Embarked"] = 'S'
data$Pclass = as.factor(data$Pclass)
data[is.na(data$Fare),"Fare"] = median(data$Fare,na.rm=T)
data$title = as.factor(data$title)
### Split into train/test
train = data[1:891,]
test = data[892:nrow(data),]
train = cbind(train,train1["Survived"])
train$Survived = as.factor(train$Survived)

trainControl = trainControl(method = 'cv',number = 10)

rfFit = train(train[,!names(train) %in% "Survived"],train[,"Survived"],method = 'rf')
DTFit = train(Survived~.,data = train,method = 'rpart')
GBTFit = train(Survived~.,data = train, method = 'gbm',trControl = trainControl,verbose = F)  

### Missing value handling - basic technique...

data[is.na(data$Age),"Age"] = median(data$Age,na.rm=T)

table(data$Embarked) ### Mode is 'S'
data[is.na(data$Embarked),"Embarked"] = 'S'
data$Pclass = as.factor(data$Pclass)
data[is.na(data$Fare),"Fare"] = median(data$Fare,na.rm=T)
data$title = as.factor(data$title)


### Some more features...


data$PassengerId = NULL # All values are unique..no info..

data$title=gsub('(.*,)|(\\..*)',"",data$Name)
data$title=gsub('^\\s+|\\s+$',"",data$title)
freq = names(sort(table(data$title),decreasing=T)[1:6])
data$title = ifelse(data$title %in% freq,data$title,"Rare")
data$title = as.factor(data$title)
data$Cabin_Present = (!is.na(data$Cabin)) * 1

data$Name = data$Cabin = data$Ticket = NULL 


data$Family = data$SibSp + data$Parch
#data$Parch = data$SibSp = NULL

data$Family_size = ifelse(data$Family==0,"Single",ifelse(data$Family>=1 & data$Family<=4,"Small","Large"))
data$Family_size = as.factor(data$Family_size


#data$Family = NULL

# data$Individual = ifelse(data$Family==0,"yes","no")
# data$Individual = as.factor(data$Individual)



### Modeling...
library(caret)
train = data[1:891,]
test = data[892:nrow(data),]
train = cbind(train,train1["Survived"])
train$Y = as.factor(train$Y)

trainControl = trainControl(method = 'cv',number = 10)


rfFit = train(train[,!names(train) %in% "Y"],train[,"Y"],method = 'rf',trControl = trainControl)
DTFit = train(Survived~.,data = train,method = 'rpart')
GBTFit = train(Survived~.,data = train, method = 'gbm',trControl = trainControl,verbose = F)  