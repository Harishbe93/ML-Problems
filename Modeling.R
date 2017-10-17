rm(list=ls())
gc()

library(randomForest)
library(caret)
setwd("E:\\Sales Analytics\\Data\\Data\\Modeling")

data=read.csv("Base data with dummiesV1.csv")
data$a=NULL

rm(list=ls())
gc()
setwd("E:\\Sales Analytics\\Modeling-Revamp")



		##Removing unwanted cols
		cols=c("Status","Lost","Opportunity_ID","Opportunity.Name","Account","Manual_Probability","Stage_based_Probability","Advisor_Name","Order_Value______In_Absolute__","Actual_ACV______In_Absolute__","Actual_Revenue______In_Absolute__","Original_Revenue______In_Absolute__","Department","SL_ACV_Bucket","Duration_Bucket")

		data=data[,!names(data) %in% cols]

		levels(data$Opportunity_Type)=c(levels(data$Opportunity_Type),"Others")
		levels(data$Engagement_Type)=c(levels(data$Engagement_Type),"Others")
		levels(data$Engagement_Model)=c(levels(data$Engagement_Model),"Others")
		levels(data$Sub_Practice)=c(levels(data$Sub_Practice),"Others")
		levels(data$Practice)=c(levels(data$Practice),"Others")

		data[which(data$Opportunity_Type==""),"Opportunity_Type"]="Others"
		data[which(data$Sub_Practice==""),"Sub_Practice"]="Others"
		data[which(data$Engagement_Model==""),"Engagement_Model"]="Others"
		data[which(data$Engagement_Type==""),"Engagement_Type"]="Others"
		data[which(data$Practice==""),"Practice"]="Others"

		data[is.na(data$Duration_Months_),"Duration_Months_"]=median(data$Duration_Months_,na.rm=T)
		### Dummy coding for log regression
		## Separate categorical and numerical

		s=sapply(data,function(x)class(x))
		cat_vars=names(s[which(s=="factor")])
		num_vars=names(s[-which(s=="factor")])

		dummy=data.frame(a=seq(1,3992,1))
			for(i in cat_vars){
				eval(parse(text=paste("temp=model.matrix(~",i,"+0,data=data)",sep="")))
				dummy=cbind(dummy,temp)
			}

		dum_data=cbind(dummy,data[,num_vars])
		dum_data[is.na(dum_data$Duration_Months_),"Duration_Months_"]=median(dum_data$Duration_Months_,na.rm=T)
		write.csv(dum_data,"Base data with dummiesV1.csv",row.names=F)

set.seed(10)
index=createDataPartition(data$Status_Reason,p=0.70,list=F)
ind=as.numeric(index)

train=data[ind,]
test=data[-ind,]

### Missing value handling

## For numeric zeros

num


rf=randomForest(factor(Status_Reason)~.,data=train)
print(rf)

p=predict(rf,test,type="prob")
prob_for_1=p[,2]

table(prob_for_1>0.5,test$Status_Reason)

## Trial and error

t=paste(n[1:3],collapse="+")
eval(parse(text=paste("lm=glm(factor(Status_Reason)~",t,",data=train,family=binomial)",sep="")))

fit=glm(Status_Reason~.,data=train,family=binomial)
n=names(train)

for(i in 1:length(n)){
	names=n[1:i]
	fit=glm(Status_Reason~.,data=train[,names(train) %in% c(names,"Status_Reason")],family=binomial(link="logit"))

}


c=coef(fit)
rem=names(c[is.na(c)])
rem=gsub("`","",rem)

fit=glm(Status_Reason~.,data=train[,!names(train) %in% rem],family=binomial)
names=names(train[,!names(train) %in% rem])

fit=glm(Status_Reason~.,data=train[,c(names[1:10],"Status_Reason")],family=binomial)
fit=glm(Status_Reason~.,data=train[,c(names[c(1:59,61:94,96:100,102)],"Status_Reason")],family=binomial,maxit=50)




d=data.frame(summary(fit)$coefficients)
dd=d[d$Pr...z..<0.05,]
final_vars=row.names(dd)
final_vars=final_vars[2:length(final_vars)]
final_vars=gsub("`","",final_vars)

fit=glm(Status_Reason~.,data=train[,c(final_vars,"Status_Reason")],family=binomial)
d=data.frame(summary(fit)$coefficients)
dd=d[d$Pr...z..<0.05,]
final_vars=row.names(dd)
final_vars=final_vars[2:length(final_vars)]
final_vars=gsub("`","",final_vars)
