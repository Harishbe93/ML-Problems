

### Required libraries
library(e1071)
library(randomForest)
library(	dplyr)
library(rpart)
library(xgboost)



rm(list=ls())
setwd('E:\\Sales Analytics\\Data\\Data\\CSVs\\')


### Reading the input file...
df=read.csv('Win loss data for analysis.csv')
names(df)=gsub("\\.","_",names(df))
dim(df)

####  Converting the revenue features to integers

vars=c("Original_Revenue______In_Absolute__","Actual_Revenue______In_Absolute__","Order_Value______In_Absolute__","Actual_ACV______In_Absolute__","SL_Actual_ACV______In_Absolute__","SL_Actual_Value______In_Absolute__")

for(i in vars){
	df[,i]=as.numeric(gsub(",","",df[,i]))
}


# df$Stage_based_Probability=NULL
# df$Manual_Probability=NULL
# df$Advisor_Name=NULL



### Removing trans.curr variables
df_bkp=df
df=df[,!names(df) %in% c("Actual_Revenue__Trans_Curr__In_Absolute_","Original_Revenue__Trans_Curr__In_Absolute_","SL_Actual_Value__Trans_Curr__In_Absolute_")]






#### Merging levels according to the new architecture

df[df$Practice=="CXO" & df$Sub_Practice=="EPM","Practice"]="Business Intelligence"

df[df$Practice=="CXO" & df$Sub_Practice=="FRC","Practice"]="Information Management"

df[df$Practice=="CXO" & df$Sub_Practice=="Marketing","Practice"]="Business Intelligence"

df[df$Practice=="CXO" & df$Sub_Practice=="Supply Chain","Practice"]="Business Intelligence"

df[df$Practice=="CXO" & df$Sub_Practice=="Opera Solutions","Practice"]="Big Data Analytics"

df[df$Practice=="MyEngine" & df$Sub_Practice=="Enterprise BI","Practice"]="Business Intelligence"

df[df$Practice=="MyEngine" & df$Sub_Practice=="Enterprise DI","Practice"]="Information Management"

df[df$Practice=="MyViews","Practice"]="Business Intelligence"

df[df$Practice=="Data Warehouse & Appliances","Practice"]="Big Data Analytics"

df[df$Practice=="MyPlatforms" & df$Sub_Practice=="Enterprise BI","Practice"]="Business Intelligence"

df[df$Practice=="MyPlatforms" & df$Sub_Practice %in% c("Analytics","Cloud Advisory & Migration Service"),"Practice"]="Data Platform Engineering"

df=df[!df$Practice %in% c("MyServices","MyWorkplace"),]

df[df$Status_Reason=="Terminated","Status_Reason"]="Lost"

df1=df[!df$Engagement_Type %in% c("Fixed Capacity"),]

df1$Engagement_Type=as.factor(as.character(df1$Engagement_Type))

df=df1
#### Doubts!!!  MyServices   AND   MyWorkplace


df$Status_Reason=as.factor(as.character(df$Status_Reason))
df$Practice=as.factor(as.character(df$Practice))


### 0/1 coding

df$Status_Reason=as.character(df$Status_Reason)
df[df$Status_Reason=="Lost","Status_Reason"]=0
df[df$Status_Reason=="Won","Status_Reason"]=1
df$Status_Reason=as.numeric(df$Status_Reason)
df$Status_Reason=as.factor(df$Status_Reason)
df$Advisor=ifelse(df$Advisor_Name=="No Advisor","No","Yes")
df$Advisor=as.factor(df$Advisor)

############ Binning

df$SL_ACV_Bucket=ifelse(df$SL_Actual_ACV______In_Absolute__<=500000,"<0.5 million",ifelse(df$SL_Actual_ACV______In_Absolute__>500000 & df$SL_Actual_ACV______In_Absolute__<1000000,"0.5 to 1 million",ifelse(df$SL_Actual_ACV______In_Absolute__>1000000 & df$SL_Actual_ACV______In_Absolute__<=3000000,"1 to 3 million",ifelse(df$SL_Actual_ACV______In_Absolute__>3000000 & df$SL_Actual_ACV______In_Absolute__<=5000000,"3 to 5 million",ifelse(df$SL_Actual_ACV______In_Absolute__<5000000 & df$SL_Actual_ACV______In_Absolute__<10000000,"5 to 10 million",">10 million")))))
df$Duration_Bucket=ifelse(df$Duration_Months_<3,"<3 months",ifelse(df$Duration_Months_>=3 & df$Duration_Months_<6,"3-6 months",ifelse(df$Duration_Months_>=6 & df$Duration_Months_<9,"6-9 months",ifelse(df$Duration_Months_>=9 & df$Duration_Months_<12,"9-12 months",">12 months"))))
df$Advisor=ifelse(df$Advisor_Name=="No Advisor","No","Yes")
df$Status=ifelse(df$Status_Reason==1,"Yes","No")
df$Lost=ifelse(df$Status_Reason==0,1,0)
write.csv(df,"test1.csv",row.names=F)

### Missing rows

df$Missing=apply(df,1,function(x) sum(is.na(x))+length(which(x=="")))

data=df[df$Missing==0,]


df=read.csv("test1.csv")
df$Status_Reason=as.factor(as.character(df$Status_Reason))

df$Stage_based_Probability=NULL
df$Manual_Probability=NULL
df$Advisor_Name=NULL

### Classifying the variables with and without missing values...
missing=sapply(df,function(x) sum(is.na(x))+length(which(x=="")))
class=sapply(df,class)
new_df=data.frame(missing,class)
cat_var=new_df[new_df$missing>0 & new_df$class %in% c("factor","character"),]
cat_var=rownames(cat_var)

good_var=names(missing[missing==0])
bad_var=names(missing[missing>0])


levels(df$Stage_based_Probability)[1]=NA
levels(df$Opportunity_Type)[1]=NA
levels(df$Advisor_Name)[1]=NA
levels(df$Engagement_Type)[1]=NA
levels(df$Practice)[1]=NA
levels(df$Sub_Practice)[1]=NA
levels(df$Engagement_Model)[1]=NA
levels(df$Department)[1]=NA

### Missing value handling using WOE

missing_val<-function(df,dep,indep){

	temp=woe(Data=df,indep,F,dep,10,Good=1,Bad=0)
	miss=temp[1,"WOE"]
	temp$new=abs(temp$WOE-miss)
	temp=temp[with(temp, order(new)), ]
	repl=temp[2,"BIN"]
	return(repl)

}

for(i in cat_var)
{
	mis=missing_val(df=df,dep="Status_Reason",indep=i)
	df[is.na(df[,i]),i]=mis
	df[which(df[,i]==""),i]=mis
}


### For numeric variables..
new_missing=sapply(df,function(x) sum(is.na(x))+length(which(x=="")))
vars=names(new_missing[new_missing>0])
for(i in vars){
	m=mean(df[,i],na.rm=T)
	df[is.na(df[,i]),i]=m
	df[which(df[,i]==""),i]=m
}

data=df

###

###
ind=sample(1:nrow(data),size=floor(0.7*nrow(data)),replace=F)
train=data[ind,]
test=data[-ind,]

### Missing value handling

### Modeling

## 1.SVM

	fit=svm(Status_Reason~.,data=train)
pred=predict(fit,test[,!names(test) %in% c("Status_Reason","Missing")])
table(pred,test$Status_Reason)

## 2.Logistic regression

fit=glm(Status_Reason~.,data=train,family=binomial(link='logit'))
pred=predict(fit,test[,!names(test) %in% c("Status_Reason","Missing")])
table(pred,test$Status_Reason)

## 3.Decision tree

data$flag=ifelse(data$SBU=="CUSTOMER",1,0)



### ALias
levels(data$SBU)=c(levels(data$SBU),"others")
data[data$SBU=="CONSUMER","SBU"]="others"
data[data$SBU=="Energy, Natural Resources, Utilities (ENU)","SBU"]="others"
data[data$SBU=="Financial Services","SBU"]="others"
data[data$SBU=="Manufacturing & Technology","SBU"]="others"

levels(data$Sub_Practice)=c(levels(data$Sub_Practice),"others")
data[data$Sub_Practice=="DPE","Sub_Practice"]="others"
data[data$Sub_Practice=="Mobile App Development - Mobile Web App Development","Sub_Practice"]="others"
data[data$Sub_Practice=="User Experience - .Net UX (User Experience)","Sub_Practice"]="others"


library(xlsx)
l=list()
k=1
new=setdiff(names(train),"Status_Reason")
fact_vars=names(class[class=="factor"])
num_vars=names(class[class=="numeric"])

for(i in fact_vars)
{
	formula=paste("Status_Reason~",i,sep="")
	fit=glm(formula,data=df,family=binomial)
	write.xlsx(coef(summary(fit)),"Without_Dummy_coding.xlsx",sheetName=i,append=T)
	
}

for(i in num_vars)
{
	formula=paste("Status_Reason~",i,sep="")
	fit=glm(formula,data=df,family=binomial)
	write.xlsx(coef(summary(fit)),"Without_Dummy_coding.xlsx",sheetName=i,append=T)
	
}


### Pairs

fit=glm(Status_Reason~Practice+Sub_Practice,data=df,family=binomial)
write.xlsx(coef(summary(fit)),"Combination.xlsx",sheetName="Practice_SubPractice",append=T)

fit=glm(Status_Reason~SBU+Vertical,data=df,family=binomial)
write.xlsx(coef(summary(fit)),"Combination.xlsx",sheetName="SBU_Vertical",append=T)


#### With interactions..

fit=glm(Status_Reason~Practice+Sub_Practice+Practice*Sub_Practice,data=df,family=binomial)
write.xlsx(coef(summary(fit)),"Combination.xlsx",sheetName="Interations--Practice_SubPractice",append=T)

fit=glm(Status_Reason~SBU+Vertical+SBU*Vertical,data=df,family=binomial)
write.xlsx(coef(summary(fit)),"Combination.xlsx",sheetName="Interations--SBU_Vertical",append=T)



# fit=glm(Status_Reason~Practice+Sub_Practice,data=df,family=binomial)
# write.xlsx(coef(summary(fit)),"Final.xlsx",sheetName="Practice_SubPractice",append=T)




fit=glm(Status_Reason~Account_Type)
train=train[,!names(train) %in% c("Order_Value______In_Absolute__","Original_Revenue______In_Absolute__")]


fact_vars=names(class[class=="factor"])

for(i in fact_vars){
	level=levels(data[,i])
	level=gsub("\\s","",level)
	for(j in level){
		eval(parse(text=paste("data['",i,"_",j,"']=","ifelse(data[,'",i,"']=='",j,"',1,0)",sep="")))
	}
}

### Factor variables...

library(xlsx)
fact_vars=names(df)[c(1,4,5,6,7,9,12,21,24:27)]
fact_vars=setdiff(fact_vars,"Status_Reason")
for(i in fact_vars){

	temp=df[,c(i,"Status_Reason")]
	temp=temp[!is.na(temp[,i]),]
	temp=temp[temp[,i]!="",]
	level=levels(temp[,i])
	#level=gsub("\\s","",level)

	if(length(level)>2){

	for(j in level){
		eval(parse(text=paste("temp['",i,"_",j,"']=","ifelse(temp[,'",i,"']=='",j,"',1,0)",sep="")))
	}
	temp[,i]=NULL
	fit=glm(Status_Reason~.,data=temp,family=binomial)
	write.xlsx(coef(summary(fit)),"With_Dummy_coding.xlsx",sheetName=i,append=T)
	}else{
	fit=glm(Status_Reason~.,data=temp,family=binomial)
	write.xlsx(coef(summary(fit)),"With_Dummy_coding.xlsx",sheetName=i,append=T)
}
}

tied_vars=c("Account_Type","Vertical","SBU")
for(i in tied_vars){
	temp=df[,c(i,"Status_Reason")]
	temp=temp[!is.na(temp[,i]),]
	temp=temp[temp[,i]!="",]
	level=levels(temp[,i])
	#level=gsub("\\s","",level)
	l=data.frame()
	if(length(level)>2){

	for(j in level){
		eval(parse(text=paste("temp['",i,"_",j,"']=","ifelse(temp[,'",i,"']=='",j,"',1,0)",sep="")))
		t=temp[,c("Status_Reason",paste(i,"_",j,sep=""))]
		fit=glm(Status_Reason~.,data=t,family=binomial)
		l=rbind(l,coef(summary(fit)))
	}
	write.xlsx(l,"Level_Modeling.xlsx",sheetName=i,append=T)
	
}

### Numeric variable...


num_vars=names(df)[c(13:16,19,20,22)]
for(i in num_vars)
{
	temp=df[,c(i,"Status_Reason")]
	temp=temp[!is.na(temp[,i]),]
	formula=paste("Status_Reason~",i,sep="")
	fit=glm(formula,data=temp,family=binomial)
	write.xlsx(coef(summary(fit)),"With_Dummy_coding.xlsx",sheetName=i,append=T)
	
}


temp=df[,c("Status_Reason","Account_Type")]
temp[,"Account_Type Existing"]=ifelse(temp$Account_Type=="Existing",1,0)
temp[,"Account_Type_Hunting"]=ifelse(temp$Account_Type=="Hunting",1,0)
temp[,"Account_Type Reserve"]=ifelse(temp$Account_Type=="Reserve",1,0)



### Bucketing 

no_of_obs=round(0.10*nrow(new))
new=new[order(-new$LR_Prob),]
new$LR_bucket1=0
start=1

for(i in 1:10){
	if(i!=10){
	new[start:(start+no_of_obs-1),"LR_bucket1"]=i
	}
	else{
		new[start:nrow(new),"LR_bucket1"]=i
	
	}
	start=start+no_of_obs
}

no_of_obs=round(0.10*nrow(new))
new=new[order(-new$Disc_Prob),]
new$Dis_bucket1=0
start=1

for(i in 1:10){
	if(i!=10){
	new[start:(start+no_of_obs-1),"Dis_bucket1"]=i
	}
	else{
		new[start:nrow(new),"Dis_bucket1"]=i
	
	}
	start=start+no_of_obs
}


#### Plotting

num_vars=names(class[class=="numeric" | class=="integer"])

for(i in num_vars){
ggplot(df,aes(x=1,y=noquote(i)))+geom_boxplot()
ggsave(paste(i,".png",sep=""))
}

ggplot(df,aes(x=Status_Reason,y=Manual_Probability))+geom_boxplot()
ggsave("Manual_Probability.png")
ggplot(df,aes(x=Status_Reason,y=Original_Revenue______In_Absolute__))+geom_boxplot()
ggsave("Original_Revenue______In_Absolute__.png")
ggplot(df,aes(x=Status_Reason,y=Actual_Revenue______In_Absolute__))+geom_boxplot()
ggsave("Actual_Revenue______In_Absolute__.png")
ggplot(df,aes(x=Status_Reason,y=Order_Value______In_Absolute__))+geom_boxplot()
ggsave("Order_Value______In_Absolute_.png")
ggplot(df,aes(x=Status_Reason,y=Actual_ACV______In_Absolute__))+geom_boxplot()
ggsave("Actual_ACV______In_Absolute__.png")
ggplot(df,aes(x=Status_Reason,y=SL_Actual_ACV______In_Absolute__))+geom_boxplot()
ggsave("SL_Actual_ACV______In_Absolute__.png")
ggplot(df,aes(x=Status_Reason,y=SL_Actual_Value______In_Absolute__))+geom_boxplot()
ggsave("SL_Actual_Value______In_Absolute__.png")
ggplot(df,aes(x=Status_Reason,y=Duration_Months_))+geom_boxplot()
ggsave("Duration_Months_.png")

### factors

ggplot(df, aes(Account_Type, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Account_Type.png")
ggplot(df, aes(Stage_based_Probability, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Stage_based_Probability.png")
ggplot(df, aes(Opportunity_Source, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Opportunity_Source.png")
ggplot(df, aes(Opportunity_Type, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Opportunity_Type.png")
ggplot(df, aes(Strategic_Deal, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Strategic_Deal.png")
ggplot(df, aes(Engagement_Type, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Engagement_Type.png")
ggplot(df, aes(Vertical, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Vertical.png")
ggplot(df, aes(SBU, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("SBU.png")
ggplot(df, aes(Geo, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Geo.png")
ggplot(df, aes(Practice, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Practice.png")
ggplot(df, aes(Sub_Practice, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Sub_Practice.png")
ggplot(df, aes(Engagement_Model, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Engagement_Model.png")
ggplot(df, aes(Department, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Department.png")
ggplot(df, aes(StormTracker_Suite_usage_status, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("StormTracker_Suite_usage_status.png")
ggplot(df, aes(Relationship_Suite_usage_status, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Relationship_Suite_usage_status.png")
ggplot(df, aes(Win_Strategy_usage_status, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Win_Strategy_usage_status.png")
ggplot(df, aes(Review_Log_usage_status)) + geom_bar(fill=Status_Reason)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Review_Log_usage_status.png")
ggplot(df, aes(Advisor, fill=Status_Reason)) + geom_bar(aes(label= (..count..)/sum(..count..))))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Advisor.png")

### Amount percentages

ggplot(df %>% group_by(Account_Type,Status_Reason) %>% summarize(a=sum(SL_Actual_Value______In_Absolute__)) %>% mutate(pct=a/sum(a),ypos = cumsum(a) - 0.5*a),aes(Account_Type, a, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))



## New with percentages

ggplot(df %>% count(Account_Type, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Account_Type, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Account_Type.png")
ggplot(df %>% count(Stage_based_Probability, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Stage_based_Probability, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Stage_based_Probability.png")
ggplot(df %>% count(Opportunity_Source, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Opportunity_Source, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Opportunity_Source.png")
ggplot(df %>% count(Opportunity_Type, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Opportunity_Type, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Opportunity_Type.png")
ggplot(df %>% count(Strategic_Deal, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Strategic_Deal, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Strategic_Deal.png")
ggplot(df %>% count(Engagement_Type, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Engagement_Type, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Engagement_Type.png")
ggplot(df %>% count(Vertical, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Vertical, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Vertical.png")
ggplot(df %>% count(SBU, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(SBU, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("SBU.png")
ggplot(df %>% count(Geo, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Geo, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Geo.png")
ggplot(df %>% count(Practice, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Practice, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Practice.png")
ggplot(df %>% count(Sub_Practice, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Sub_Practice, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Sub_Practice.png")
ggplot(df %>% count(Engagement_Model, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Engagement_Model, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Engagement_Model.png")
ggplot(df %>% count(Department, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Department, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Department.png")
ggplot(df %>% count(StormTracker_Suite_usage_status, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(StormTracker_Suite_usage_status, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("StormTracker_Suite_usage_status.png")
ggplot(df %>% count(Relationship_Suite_usage_status, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Relationship_Suite_usage_status, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Relationship_Suite_usage_status.png")
ggplot(df %>% count(Win_Strategy_usage_status, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Win_Strategy_usage_status, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Win_Strategy_usage_status.png")
ggplot(df %>% count(Review_Log_usage_status, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Review_Log_usage_status, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Review_Log_usage_status.png")
ggplot(df %>% count(Advisor, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Advisor, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Advisor.png")




### Filter and model 
level=levels(df$Practice)
k="Sub_Practice"
for(i in level){
	temp=df[df$Practice==i,c("Status_Reason","Sub_Practice")]
	levels=levels(temp[,"Sub_Practice"])
		if(length(levels)>2){

		for(j in levels){
			eval(parse(text=paste("temp['",k,"_",j,"']=","ifelse(temp[,'",k,"']=='",j,"',1,0)",sep="")))
			#t=temp[,c("Status_Reason",paste(i,"_",j,sep=""))]
			#fit=glm(Status_Reason~.,data=t,family=binomial)
			
		}
	temp[,"Sub_Practice"]=NULL
	fit=glm(Status_Reason~.,data=temp,family=binomial)
	write.xlsx(coef(summary(fit)),"New_Res.xlsx",sheetName="New",append=T)
}
}

level=levels(df$SBU)
k="Vertical"
for(i in level){
	temp=df[df$SBU==i,c("Status_Reason","Vertical")]
	levels=levels(temp[,"Vertical"])
		if(length(levels)>2){

		for(j in levels){
			eval(parse(text=paste("temp['",k,"_",j,"']=","ifelse(temp[,'",k,"']=='",j,"',1,0)",sep="")))
			#t=temp[,c("Status_Reason",paste(i,"_",j,sep=""))]
			#fit=glm(Status_Reason~.,data=t,family=binomial)
			
		}
	temp[,"Vertical"]=NULL
	fit=glm(Status_Reason~.,data=temp,family=binomial)
	write.xlsx(coef(summary(fit)),"New_Res1.xlsx",sheetName=i,append=T)
}
}

### Chaid add vars

temp$Add1=ifelse(temp[,"Practice_Big Data Analytics"]=="0" & temp[,"Practice_Information Management"]=="0",1,0)
temp$Add2=ifelse(temp[,"Practice_Big Data Analytics"]=="0" & temp[,"Practice_Information Management"]=="1",1,0)
temp$Add3=ifelse(temp[,"Practice_Big Data Analytics"]=="1",1,0)


for(i in setdiff(names(temp),"Status_Reason")){
	temp[,i]=as.integer(as.character(temp[,i]))
}

n=names(temp1)

for(i in n){
	temp1[,i]=as.factor(temp1[,i])
}

#### 6 terminals for practice/SubPractice


temp=df[,c("Status_Reason","Practice","Sub_Practice")]

levels=levels(temp[,"Practice"])
k="Practice"
if(length(levels)>2){

		for(j in levels){
			eval(parse(text=paste("temp['",k,"_",j,"']=","ifelse(temp[,'",k,"']=='",j,"',1,0)",sep="")))
			
		}
	temp[,"Practice"]=NULL
}

levels=levels(temp[,"Sub_Practice"])
k="Sub_Practice"
if(length(levels)>2){

		for(j in levels){
			eval(parse(text=paste("temp['",k,"_",j,"']=","ifelse(temp[,'",k,"']=='",j,"',1,0)",sep="")))
			
		}
	temp[,"Sub_Practice"]=NULL
}


##Rules

format_rules <- function(object, ...) {
    ft <- fitted(object)
    ns <- tapply(ft[[2]], ft[[1]], length)
    pr <- tapply(ft[[2]], ft[[1]], function(y)
      min(prop.table(table(y))))
    lb <- tapply(ft[[2]], ft[[1]], function(y)
      names(sort(table(y), decreasing = TRUE))[1])
    rl <- partykit:::.list.rules.party(object)
    paste0(rl, ": ", lb, " (n = ", ns, ", ", round(100 * pr, 2), "%)")
 }
 writeLines(format_rules(chaid))


# chaid=chaid(Status_Reason~.,data=temp,control=chaid_control(minbucket=500,minprob=0.1))
# plot(chaid)
chaid <- rpart(Status_Reason~.,data = temp, method="class",cp=0.0001)
plot(chaid, uniform=TRUE, main="Classification Tree") 
text(chaid, use.n=TRUE, all=TRUE, cex=.6)
# #old
# temp$Add1=ifelse(temp[,"Practice_Big Data Analytics"]=="0" & temp[,"Sub_Practice_Enterprise DI"]=="0" & temp[,"Practice_Data Warehouse & Appliances"]=="0" & temp[,"Sub_Practice_Business Objects"]=="0",1,0)
# temp$Add2=ifelse(temp[,"Practice_Big Data Analytics"]=="0" & temp[,"Sub_Practice_Enterprise DI"]=="0" & temp[,"Practice_Data Warehouse & Appliances"]=="0" & temp[,"Sub_Practice_Business Objects"]=="1",1,0)
# temp$Add3=ifelse(temp[,"Practice_Big Data Analytics"]=="0" & temp[,"Sub_Practice_Enterprise DI"]=="0" & temp[,"Practice_Data Warehouse & Appliances"]=="1" & temp[,"Sub_Practice_Teradata"]=="0",1,0)
# temp$Add4=ifelse(temp[,"Practice_Big Data Analytics"]=="0" & temp[,"Sub_Practice_Enterprise DI"]=="0" & temp[,"Practice_Data Warehouse & Appliances"]=="1" & temp[,"Sub_Practice_Teradata"]=="1",1,0)
# temp$Add5=ifelse(temp[,"Practice_Big Data Analytics"]=="0" & temp[,"Sub_Practice_Enterprise DI"]=="1",1,0)
# temp$Add6=ifelse(temp[,"Practice_Big Data Analytics"]=="1",1,0)

#new
temp$Add1=ifelse(temp[,"Sub_Practice_Teradata"]>=0.5,1,0)
temp$Add2=ifelse(temp[,"Sub_Practice_Teradata"]<0.5& temp[,"Sub_Practice_Advanced Analytics"]<0.5 & temp[,"Sub_Practice_Data Warehouse & Databases"]<0.5 & temp[,"Sub_Practice_Oracle BI"]<0.5 & temp[,"Sub_Practice_SAP-BW"]<0.5 & temp[,"Sub_Practice_MicroStrategy"]>=0.5,1,0)
temp$Add3=ifelse(temp[,"Sub_Practice_Teradata"]<0.5 & temp[,"Sub_Practice_Advanced Analytics"]<0.5 & temp[,"Sub_Practice_Data Warehouse & Databases"]<0.5 & temp[,"Sub_Practice_Oracle BI"]>=0.5,1,0)
temp$Add4=ifelse(temp[,"Sub_Practice_Teradata"]<0.5 & temp[,"Sub_Practice_Advanced Analytics"]<0.5 & temp[,"Sub_Practice_Data Warehouse & Databases"]>=0.5,1,0)
temp$Add5=ifelse(temp[,"Sub_Practice_Teradata"]<0.5 & temp[,"Sub_Practice_Advanced Analytics"]<0.5 & temp[,"Sub_Practice_Data Warehouse & Databases"]<0.5 & temp[,"Sub_Practice_Oracle BI"]<0.5 & temp[,"Sub_Practice_SAP-BW"]>=0.5,1,0)
temp$Add6=ifelse(temp[,"Sub_Practice_Teradata"]<0.5 & temp[,"Sub_Practice_Advanced Analytics"]<0.5 & temp[,"Sub_Practice_Data Warehouse & Databases"]<0.5 & temp[,"Sub_Practice_Oracle BI"]<0.5 & temp[,"Sub_Practice_SAP-BW"]<0.5 & temp[,"Sub_Practice_MicroStrategy"]<0.5,1,0)
temp$Add7=ifelse(temp[,"Sub_Practice_Teradata"]<0.5 & temp[,"Sub_Practice_Advanced Analytics"]>=0.5,1,0)

fit1=glm(Status_Reason~.,data=temp,family=binomial)
## SBU - Vertical

temp=df[,c("Status_Reason","SBU","Vertical")]

levels=levels(temp[,"SBU"])
k="SBU"
if(length(levels)>2){

		for(j in levels){
			eval(parse(text=paste("temp['",k,"_",j,"']=","ifelse(temp[,'",k,"']=='",j,"',1,0)",sep="")))
			
		}
	temp[,"SBU"]=NULL
}

levels=levels(temp[,"Vertical"])
k="Vertical"
if(length(levels)>2){
		for(j in levels){
			eval(parse(text=paste("temp['",k,"_",j,"']=","ifelse(temp[,'",k,"']=='",j,"',1,0)",sep="")))
			
		}
	temp[,"Vertical"]=NULL
}


	chaid <- rpart(Status_Reason~.,data = temp, method="class",cp=0.01)
plot(chaid, uniform=TRUE, main="Classification Tree") 
text(chaid, use.n=TRUE, all=TRUE, cex=.6) # visualization


temp$Add1=ifelse(temp[,"Vertical_Retail"]<0.5 & temp[,"Vertical_Consumer Electronics & Peripherals"]>=0.5,1,0)
temp$Add2=ifelse(temp[,"Vertical_Retail"]<0.5 & temp[,"Vertical_Consumer Electronics & Peripherals"]<0.5 & 	temp[,"Vertical_Pharma US"]>=0.5,1,0)
temp$Add3=ifelse(temp[,"Vertical_Retail"]>=0.5,1,0)
temp$Add4=ifelse(temp[,"Vertical_Retail"]<0.5 & temp[,"Vertical_Consumer Electronics & Peripherals"]<0.5 & temp[,"Vertical_Pharma US"]<0.5,1,0)

fit=glm(Status_Reason~.,data=temp,family=binomial)

chaid <- rpart(Status_Reason~.,data = temp, method="class",cp=0.0001)
plot(chaid, uniform=TRUE, main="Classification Tree") 
text(chaid, use.n=TRUE, all=TRUE, cex=.6)

chaid=chaid(Status_Reason~.,data=temp,control=chaid_control(minbucket=500,minprob=0.1))
plot(chaid)


temp$Add1=ifelse(temp[,"Vertical_Retail"]=="0",1,0)
temp$Add2=ifelse(temp[,"Vertical_Retail"]=="1",1,0)


write.xlsx(coef(summary(fit)),"Chaid.xlsx",sheetName="Practice",append=T)
write.xlsx(coef(summary(fir)),"Chaid.xlsx",sheetName="SBU",append=T)


### Individual level modeling...


vars=c("Engagement_Type","Geo","Engagement_Model","Advisor")
for(i in vars){
	temp=df[,c("Status_Reason",i)]
	levels=levels(temp[,i])
l=data.frame()
	for(j in levels){
		eval(parse(text=paste("temp['",i,"_",j,"']=","ifelse(temp[,'",i,"']=='",j,"',1,0)",sep="")))
		t=temp[,c("Status_Reason",paste(i,"_",j,sep=""))]
		fit=glm(Status_Reason~.,data=t,family=binomial)
		l=rbind(l,coef(summary(fit)))
	}
	write.xlsx(l,"New_Level.xlsx",sheetName=i,append=T)

}


### 5 million benchmark
new=df[df$SL_Actual_Value______In_Absolute_>5000000,]
vars=c("StormTracker_Suite_usage_status","Relationship_Suite_usage_status","Win_Strategy_usage_status","Review_Log_usage_status")
for(i in vars){
	f=paste("Status_Reason~",i,sep="")
	fit=glm(f,data=new,family=binomial)
	write.xlsx(coef(summary(fit)),"5Mill.xlsx",sheetName=i,append=T)
}

ggplot(new %>% count(StormTracker_Suite_usage_status, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(StormTracker_Suite_usage_status, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("StormTracker_Suite_usage_status.png")
ggplot(new %>% count(Relationship_Suite_usage_status, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Relationship_Suite_usage_status, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Relationship_Suite_usage_status.png")
ggplot(new %>% count(Win_Strategy_usage_status, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Win_Strategy_usage_status, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Win_Strategy_usage_status.png")
ggplot(new %>% count(Review_Log_usage_status, Status_Reason) %>%  mutate(pct=n/sum(n),ypos = cumsum(n) - 0.5*n),aes(Review_Log_usage_status, n, fill=Status_Reason)) + geom_bar(stat="identity") + geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"),y=ypos))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Review_Log_usage_status.png")

ggplot(new, aes(StormTracker_Suite_usage_status, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("StormTracker_Suite_usage_status.png")
ggplot(new, aes(Relationship_Suite_usage_status, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Relationship_Suite_usage_status.png")
ggplot(new, aes(Win_Strategy_usage_status, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Win_Strategy_usage_status.png")
ggplot(new, aes(Review_Log_usage_status, fill=Status_Reason)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("Review_Log_usage_status.png")

### Getting the range of good observations...


str=boxplot(df$Original_Revenue______In_Absolute__)$stats[c(1,5),]
str=as.character(str)
m=as.integer(str[2])
dummy=df$Original_Revenue______In_Absolute__[df$Original_Revenue______In_Absolute__>m & df$Status_Reason=="1"]
mm=sort(dummy,decreasing=T)
values=mm[1:5]

df1=df[,c("Status_Reason","Account_Type")]
df1=ddply(df1, .(Account_Type), transform, percent = sum(Status_Reason))
df1$Perc=0
df1[df1$Account_Type=="Existing","percent"]=df1[df1$Account_Type=="Existing","percent"]/3291
df1[df1$Account_Type=="Hunting","percent"]=df1[df1$Account_Type=="Hunting","percent"]/659
df1[df1$Account_Type=="Reserve","percent"]=df1[df1$Account_Type=="Reserve","percent"]/44


### With percentage labels...

ggplot(df %>% count(Account_Type, Status_Reason) %>%    
         mutate(pct=n/sum(n),              
                ypos = cumsum(n) - 0.5*n),  
       aes(Account_Type, n, fill=Status_Reason)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%"), y=ypos))