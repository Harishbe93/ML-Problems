# workspace cleanup
rm(list=ls())

# Loading train and test data sets
trainData <- read.csv(file.choose(), header = T)
pipelineData <- read.csv(file.choose(), header = T)
str(trainData)

# Checking for NA's on the entire data
lr <- glm(Status.Reason~.,data=trainData,family = binomial(link = "logit"),maxit=100)
summary(lr)

#Removing NA's from the data and applying regression
lr2 <-
  glm(
    Status.Reason ~ Account.Type.Existing + Account.Type.Hunting + Account.Classification.Flow +
      Account.Classification.Growth + Account.Classification.Mega.Gama + Account.Classification.Nurture +
      Stage.Buying + Stage.Closing + Stage.Creating + Stage.Securing + Stage.Shaping +
      Opportunity.Source.Customer.Initiative + Opportunity.Source.Lead + Opportunity.Source.Public.Records +
      Opportunity.Type.Incremental.Enhancement + Opportunity.Type.New + Proposal.Type.BAFO...Best.And.Final.Offer.. +
      Proposal.Type.Empanelment..MSA + Proposal.Type.Proactive.Proposal + Proposal.Type.RFI +
      Proposal.Type.RFP.ITT + Proposal.Type.RFR + Engagement.Type.Co.Sourcing..L2. +
      Engagement.Type.Consulting..L4. + Engagement.Type.Staff.Augmentation..L1. +
      Engagement.Type.Wipro.Managed..L3. + SBU.COMMUNICATIONS + SBU.CONSUMER +
      SBU.Energy..Natural.Resources..Utilities..ENU. + SBU.Financial.Services..FS. +
      SBU.Healthcare..LifeSciences...Services..HLS. + Geo.EUROPE + Geo.Growth.Markets +
      Geo.INDIA + Practice.Big.Data.Analytics + Practice.Business.Intelligence +
      Practice.Data.Platform.Engineering + Practice.Data.Warehouse...Appliances +
      Practice.Information.Management + Engagement.Model.Development + Engagement.Model.Support +
      Pricing.Involved + StormTracker.Suite.usage.status + Relationship.Suite.usage.status +
      Win.Strategy.usage.status + Review.Log.usage.status,data = trainData,family = binomial(link = "logit"),maxit =
      100
  )
summary(lr2)


#Removed stage variables from the above model
lr3 <- glm(Status.Reason~Account.Type.Existing+Account.Type.Hunting+Account.Classification.Flow+Account.Classification.Growth+Account.Classification.Mega.Gama+Account.Classification.Nurture+Opportunity.Source.Customer.Initiative+Opportunity.Source.Lead+Opportunity.Source.Public.Records+Opportunity.Type.Incremental.Enhancement+Opportunity.Type.New+Proposal.Type.BAFO...Best.And.Final.Offer..+Proposal.Type.Empanelment..MSA+Proposal.Type.Proactive.Proposal+Proposal.Type.RFI+Proposal.Type.RFP.ITT+Proposal.Type.RFR+Engagement.Type.Co.Sourcing..L2.+Engagement.Type.Consulting..L4.+Engagement.Type.Staff.Augmentation..L1.+Engagement.Type.Wipro.Managed..L3.+SBU.COMMUNICATIONS+SBU.CONSUMER+SBU.Energy..Natural.Resources..Utilities..ENU.+SBU.Financial.Services..FS.+SBU.Healthcare..LifeSciences...Services..HLS.+Geo.EUROPE+Geo.Growth.Markets+Geo.INDIA+Practice.Big.Data.Analytics+Practice.Business.Intelligence+Practice.Data.Platform.Engineering+Practice.Data.Warehouse...Appliances+Practice.Information.Management+Engagement.Model.Development+Engagement.Model.Support+Pricing.Involved+StormTracker.Suite.usage.status+Relationship.Suite.usage.status+Win.Strategy.usage.status+Review.Log.usage.status,data=trainData,family = binomial(link = "logit"),maxit=100)
summary(lr3)

#Checking VIF 
lm3 <- lm(Status.Reason~Account.Type.Existing+Account.Type.Hunting+Account.Classification.Flow+Account.Classification.Growth+Account.Classification.Mega.Gama+Account.Classification.Nurture+Opportunity.Source.Customer.Initiative+Opportunity.Source.Lead+Opportunity.Source.Public.Records+Opportunity.Type.Incremental.Enhancement+Opportunity.Type.New+Proposal.Type.BAFO...Best.And.Final.Offer..+Proposal.Type.Empanelment..MSA+Proposal.Type.Proactive.Proposal+Proposal.Type.RFI+Proposal.Type.RFP.ITT+Proposal.Type.RFR+Engagement.Type.Co.Sourcing..L2.+Engagement.Type.Consulting..L4.+Engagement.Type.Staff.Augmentation..L1.+Engagement.Type.Wipro.Managed..L3.+SBU.COMMUNICATIONS+SBU.CONSUMER+SBU.Energy..Natural.Resources..Utilities..ENU.+SBU.Financial.Services..FS.+SBU.Healthcare..LifeSciences...Services..HLS.+Geo.EUROPE+Geo.Growth.Markets+Geo.INDIA+Practice.Big.Data.Analytics+Practice.Business.Intelligence+Practice.Data.Platform.Engineering+Practice.Data.Warehouse...Appliances+Practice.Information.Management+Engagement.Model.Development+Engagement.Model.Support+Pricing.Involved+StormTracker.Suite.usage.status+Relationship.Suite.usage.status+Win.Strategy.usage.status+Review.Log.usage.status,data=trainData)
summary(lm3)
step(lr3)

library(MASS)
library(car)
vif(lm3)
write.csv(vif(lm3), file="D:/CTO/CTO wrk/Q2'17/Data Science for STrategy/trace reports/Round 3/Main modeling/outcome_VIF.csv")

#Model with VIF<10

lr4 <- glm(Status.Reason ~ Opportunity.Source.Customer.Initiative+Opportunity.Source.Lead+Opportunity.Source.Public.Records+Opportunity.Type.Incremental.Enhancement+Opportunity.Type.New+Proposal.Type.BAFO...Best.And.Final.Offer..+Proposal.Type.Empanelment..MSA+Proposal.Type.RFI+Proposal.Type.RFR+Engagement.Type.Co.Sourcing..L2.+Engagement.Type.Consulting..L4.+Engagement.Type.Staff.Augmentation..L1.+SBU.COMMUNICATIONS+SBU.CONSUMER+SBU.Energy..Natural.Resources..Utilities..ENU.+SBU.Financial.Services..FS.+SBU.Healthcare..LifeSciences...Services..HLS.+Geo.EUROPE+Geo.Growth.Markets+Geo.INDIA+Practice.Big.Data.Analytics+Practice.Business.Intelligence+Practice.Data.Platform.Engineering+Practice.Data.Warehouse...Appliances+Practice.Information.Management+Pricing.Involved+StormTracker.Suite.usage.status+Relationship.Suite.usage.status+Win.Strategy.usage.status+Review.Log.usage.status ,data = trainData,family = binomial(link = "logit"),maxit =100)
summary(lr4)

# Model with only significant variables

lr5 <-
  glm(
    Status.Reason ~ Opportunity.Type.New + Proposal.Type.BAFO...Best.And.Final.Offer.. +
      Engagement.Type.Co.Sourcing..L2.+Engagement.Type.Consulting..L4.+SBU.COMMUNICATIONS+ SBU.Energy..Natural.Resources..Utilities..ENU. +
      Geo.Growth.Markets + Pricing.Involved,data = trainData,family = binomial(link = "logit"),maxit =
      100
  )
summary(lr5)

lm5 <-
  lm(
    Status.Reason ~ Opportunity.Type.New + Proposal.Type.BAFO...Best.And.Final.Offer.. +
      Engagement.Type.Co.Sourcing..L2.+Engagement.Type.Consulting..L4.+SBU.COMMUNICATIONS+ SBU.Energy..Natural.Resources..Utilities..ENU. +
      Geo.Growth.Markets + Pricing.Involved,data = trainData)
vif(lm5)

str(lr5)
output <- predict(lr5,trainData,type="response")
write.csv(output, file="D:/CTO/CTO wrk/Q2'17/Data Science for STrategy/trace reports/Round 3/Main modeling/prob_train_LR.csv")


# Concordance test

concordance <- read.csv(file.choose(), header = T)
bruteforce<-function(Concordance)
{
  Data = cbind(trainData$Status.Reason, concordance$x) 
  ones = Data[Data[,1] == 1,]
  zeros = Data[Data[,1] == 0,]
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  Pairs=dim(zeros)[1]*dim(ones)[1]
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100 
}

# KS Chart

fitted=concordance$x;

bin=10
score=data.frame(probability=trainData$new,response=trainData$Status.Reason)
score=score[order(-score$probability),]
row.names(score)=1:nrow(score)
score$bucket=floor(bin*(as.numeric(row.names(score))-1)/nrow(score))+1

library(sqldf)
Bucket_Details=sqldf(' select bucket,
                     count(*) as COUNT,
                     min(probability) as MINSCORE,
                     max(probability) as MAXSCORE,
                     (avg(probability)*100) as PREDDEFRATE,
                     sum(response) as WIN,
                     count(*)- sum(response) as LOSS
                     from score
                     group by 1')


Bucket_Details$CUM_COUNT=cumsum(Bucket_Details$COUNT)
Bucket_Details$CUM_WIN=cumsum(Bucket_Details$WIN)
Bucket_Details$WIN_CAPTRED=(Bucket_Details$WIN/sum(Bucket_Details$WIN))*100
Bucket_Details$ACTUAL_WIN_RATE=(Bucket_Details$WIN/Bucket_Details$COUNT)*100
Bucket_Details$CUM_ACTUAL_WIN=(Bucket_Details$CUM_WIN/Bucket_Details$CUM_COUNT)*100
Bucket_Details$CUM_LOSS=cumsum(Bucket_Details$LOSS)
Bucket_Details$CUM_PER_WIN=(Bucket_Details$CUM_WIN/sum(Bucket_Details$WIN))*100
Bucket_Details$CUM_PER_LOSS=(Bucket_Details$CUM_LOSS/sum(Bucket_Details$LOSS))*100
Bucket_Details$KS=Bucket_Details$CUM_PER_WIN-Bucket_Details$CUM_PER_LOSS
write.csv(Bucket_Details,file="D:/CTO/CTO wrk/Q2'17/Data Science for STrategy/trace reports/Round 3/Main modeling/ks.csv",row.names=FALSE)

# Adding chaid rules to the train data

trainData$Add1 <- ifelse(trainData$Opportunity.Type.New< 0.5,1,0)
summary(trainData$Add1)
trainData$Add2 <- ifelse(trainData$Opportunity.Type.New>=0.5 & trainData$Proposal.Type.BAFO...Best.And.Final.Offer..< 0.5 & trainData$Proposal.Type.Proactive.Proposal< 0.5 & trainData$SBU.Energy..Natural.Resources..Utilities..ENU.< 0.5 & trainData$ Proposal.Type.RFI< 0.5 & trainData$ Account.Classification.Nurture>=0.5 & trainData$Proposal.Type.RFR< 0.5 & trainData$Account.Type.Hunting< 0.5 & trainData$Review.Log.usage.status< 0.5,1,0)
trainData$Add3 <- ifelse(trainData$Opportunity.Type.New>=0.5 & trainData$Proposal.Type.BAFO...Best.And.Final.Offer..< 0.5 & trainData$Proposal.Type.Proactive.Proposal< 0.5 & trainData$SBU.Energy..Natural.Resources..Utilities..ENU.< 0.5 & trainData$Proposal.Type.RFI< 0.5 & trainData$Account.Classification.Nurture< 0.5,1,0)
trainData$Add4 <- ifelse(trainData$Opportunity.Type.New>=0.5 & trainData$Proposal.Type.BAFO...Best.And.Final.Offer..< 0.5 & trainData$Proposal.Type.Proactive.Proposal< 0.5 & trainData$SBU.Energy..Natural.Resources..Utilities..ENU.< 0.5 & trainData$Proposal.Type.RFI< 0.5 & trainData$Account.Classification.Nurture>=0.5 & trainData$Proposal.Type.RFR< 0.5 & trainData$Account.Type.Hunting>=0.5,1,0)
str(trainData)
write.csv(trainData,file="D:/CTO/CTO wrk/Q2'17/Data Science for STrategy/trace reports/Round 3/Main modeling/data with added variables.csv",row.names=FALSE)

#LR with added variables
lr6 <-
  glm(
    Status.Reason ~ Add1+Add2+Add3+Add4+Opportunity.Type.New + Proposal.Type.BAFO...Best.And.Final.Offer.. +
      Engagement.Type.Co.Sourcing..L2.+Engagement.Type.Consulting..L4.+SBU.COMMUNICATIONS+ SBU.Energy..Natural.Resources..Utilities..ENU. +
      Geo.Growth.Markets + Pricing.Involved,data = trainData,family = binomial(link = "logit"),maxit =
      100
  )
summary(lr6)

# Final model 

lr7 <-
  glm(
    Status.Reason ~ Add1+Add3+Add4+Proposal.Type.BAFO...Best.And.Final.Offer..+Geo.Growth.Markets + Pricing.Involved,data = trainData,family = binomial(link = "logit"),maxit =
      100
  )
summary(lr7)

# Final model - II

lr8 <-
  glm(
    Status.Reason ~ Add1+Add3+ Proposal.Type.BAFO...Best.And.Final.Offer.. +
      Engagement.Type.Co.Sourcing..L2.+Geo.Growth.Markets + Pricing.Involved,data = trainData,family = binomial(link = "logit"),maxit =
      100
  )
summary(lr8)

# Final model used for prediction

lr9 <-
  glm(
    Status.Reason ~ Add1+Add3+ Proposal.Type.BAFO...Best.And.Final.Offer..+
     Geo.Growth.Markets + Pricing.Involved,data = trainData,family = binomial(link = "logit"),maxit =
      100
  )
summary(lr9)

output <- predict(lr9,test,type="response")
write.csv(output, file="D:/CTO/CTO wrk/Q2'17/Data Science for STrategy/trace reports/Round 3/Main modeling/prob_train_data_final_model3.csv")

# Converting variables to dummy variables
pipelineData$Account.Type.Existing <- ifelse(pipelineData$Account.Type== "Existing",1,0)
pipelineData$Account.Type.Hunting <- ifelse(pipelineData$Account.Type== "Hunting",1,0)
pipelineData$Account.Type.Reserve <- ifelse(pipelineData$Account.Type== "Reserve",1,0)
pipelineData$Account.Classification.Flow <- ifelse(pipelineData$Account.Classification== "Flow",1,0)
pipelineData$Account.Classification.Growth <- ifelse(pipelineData$Account.Classification== "Growth",1,0)
pipelineData$Account.Classification.Mega.Gama <- ifelse(pipelineData$Account.Classification== "Mega / Gama",1,0)
pipelineData$Account.Classification.Nurture <- ifelse(pipelineData$Account.Classification== "Nurture",1,0)
pipelineData$Account.Classification.RestOfMarket <- ifelse(pipelineData$Account.Classification== "Rest of market",1,0)
pipelineData$Account.Classification.Tail <- ifelse(pipelineData$Account.Classification== "Tail",1,0)
pipelineData$Opportunity.Source.Customer.Initiative <- ifelse(pipelineData$Opportunity.Source== "Customer Initiative",1,0)
pipelineData$Opportunity.Source.Lead <- ifelse(pipelineData$Opportunity.Source== "Lead",1,0)
pipelineData$Opportunity.Source.Public.Records <- ifelse(pipelineData$Opportunity.Source== "Public Records",1,0)
pipelineData$Opportunity.Source.WiproInitiative <- ifelse(pipelineData$Opportunity.Source== "Wipro Initiative",1,0)
pipelineData$Opportunity.Type.Incremental.Enhancement <- ifelse(pipelineData$Opportunity.Type== "Incremental/Enhancement",1,0)
pipelineData$Opportunity.Type.New <- ifelse(pipelineData$Opportunity.Type== "New",1,0)
pipelineData$Opportunity.Type.Renewal <- ifelse(pipelineData$Opportunity.Type== "Renewal",1,0)
pipelineData$Proposal.Type.BAFO...Best.And.Final.Offer..<- ifelse(pipelineData$Proposal.Type== "BAFO ( Best And Final Offer )",1,0)
pipelineData$Proposal.Type.Empanelment..MSA<- ifelse(pipelineData$Proposal.Type== "Empanelment/ MSA",1,0)
pipelineData$Proposal.Type.Proactive.Proposal<- ifelse(pipelineData$Proposal.Type== "Proactive Proposal",1,0)
pipelineData$Proposal.Type.RFI<- ifelse(pipelineData$Proposal.Type== "RFI",1,0)
pipelineData$Proposal.Type.RFP.ITT<- ifelse(pipelineData$Proposal.Type== "RFP/ ITT",1,0)
pipelineData$Proposal.Type.RFR<- ifelse(pipelineData$Proposal.Type== "RFR",1,0)
pipelineData$Engagement.Type.Co.Sourcing..L2.<- ifelse(pipelineData$Engagement.Type== "Co Sourcing (L2)",1,0)
pipelineData$Engagement.Type.Consulting..L4.<- ifelse(pipelineData$Engagement.Type== "Consulting (L4)",1,0)
pipelineData$Engagement.Type.Staff.Augmentation..L1.<- ifelse(pipelineData$Engagement.Type== "Staff Augmentation (L1)",1,0)
pipelineData$Engagement.Type.Wipro.Managed..L3.<- ifelse(pipelineData$Engagement.Type== "Wipro Managed (L3)",1,0)
pipelineData$SBU.COMMUNICATIONS<- ifelse(pipelineData$SBU== "COMMUNICATIONS",1,0)
pipelineData$SBU.CONSUMER<- ifelse(pipelineData$SBU== "CONSUMER",1,0)
pipelineData$SBU.Energy..Natural.Resources..Utilities..ENU.<- ifelse(pipelineData$SBU== "Energy, Natural Resources, Utilities (ENU)",1,0)
pipelineData$SBU.Financial.Services..FS.<- ifelse(pipelineData$SBU== "Financial Services (FS)",1,0)
pipelineData$SBU.Healthcare..LifeSciences...Services..HLS.<- ifelse(pipelineData$SBU== "Healthcare, LifeSciences & Services (HLS)",1,0)
pipelineData$SBU.ManufacturingTechnology<- ifelse(pipelineData$SBU== "Manufacturing & Technology",1,0)
pipelineData$Geo.EUROPE<- ifelse(pipelineData$Geo== "EUROPE",1,0)
pipelineData$Geo.Growth.Markets<- ifelse(pipelineData$Geo== "Growth Markets",1,0)
pipelineData$Geo.USA<- ifelse(pipelineData$Geo== "USA",1,0) 
pipelineData$Practice.Big.Data.Analytics<- ifelse(pipelineData$Practice== "Big Data Analytics",1,0)
pipelineData$Practice.Business.Intelligence<- ifelse(pipelineData$Practice== "Business Intelligence",1,0)
pipelineData$Practice.Data.Platform.Engineering<- ifelse(pipelineData$Practice== "Data Platform Engineering",1,0)
pipelineData$Practice.Data.Warehouse...Appliances<- ifelse(pipelineData$Practice== "Data Warehouse & Appliances",1,0)
pipelineData$Practice.Information.Management<- ifelse(pipelineData$Practice== "Information Management",1,0)
pipelineData$Practice.WA.Database<- ifelse(pipelineData$Practice== "WA-Database",1,0)
pipelineData$Engagement.Model.Development<- ifelse(pipelineData$Engagement.Model=="Development",1,0)
pipelineData$Engagement.Model.Support<- ifelse(pipelineData$Engagement.Model=="Support",1,0)
pipelineData$Pricing.Involved <- ifelse(pipelineData$Pricing.Involved=="Yes",1,0)


# Adding new variables to the pipeline data

pipelineData$Add1 <- ifelse(pipelineData$Opportunity.Type.New< 0.5,1,0)
pipelineData$Add2 <- ifelse(pipelineData$Opportunity.Type.New>=0.5 & pipelineData$Proposal.Type.BAFO...Best.And.Final.Offer..< 0.5 & pipelineData$Proposal.Type.Proactive.Proposal< 0.5 & pipelineData$SBU.Energy..Natural.Resources..Utilities..ENU.< 0.5 & pipelineData$ Proposal.Type.RFI< 0.5 & pipelineData$ Account.Classification.Nurture>=0.5 & pipelineData$Proposal.Type.RFR< 0.5 & pipelineData$Account.Type.Hunting< 0.5 & pipelineData$Review.Log.usage.status< 0.5,1,0)
pipelineData$Add3 <- ifelse(pipelineData$Opportunity.Type.New>=0.5 & pipelineData$Proposal.Type.BAFO...Best.And.Final.Offer..< 0.5 & pipelineData$Proposal.Type.Proactive.Proposal< 0.5 & pipelineData$SBU.Energy..Natural.Resources..Utilities..ENU.< 0.5 & pipelineData$Proposal.Type.RFI< 0.5 & pipelineData$Account.Classification.Nurture< 0.5,1,0)
pipelineData$Add4 <- ifelse(pipelineData$Opportunity.Type.New>=0.5 & pipelineData$Proposal.Type.BAFO...Best.And.Final.Offer..< 0.5 & pipelineData$Proposal.Type.Proactive.Proposal< 0.5 & pipelineData$SBU.Energy..Natural.Resources..Utilities..ENU.< 0.5 & pipelineData$Proposal.Type.RFI< 0.5 & pipelineData$Account.Classification.Nurture>=0.5 & pipelineData$Proposal.Type.RFR< 0.5 & pipelineData$Account.Type.Hunting>=0.5,1,0)
str(pipelineData)
write.csv(pipelineData,file="D:/CTO/CTO wrk/Q2'17/Data Science for STrategy/trace reports/Round 3/Main modeling/pipeline data with added variables.csv",row.names=FALSE)
str(pipelineData$Pricing.Involved)

output <- predict(lr9,pipelineData,type="response")
write.csv(output, file="D:/CTO/CTO wrk/Q2'17/Data Science for STrategy/trace reports/Round 3/Main modeling/Pipeline report current prediction for Pallab/prob_pipeline_data_modelfinal.csv")

# Checking concordance and KS lift on the final model
concordance <- read.csv(file.choose(), header = T)
bruteforce<-function(Concordance)
{
  Data = cbind(trainData$Status.Reason, trainData$LDA_Prob) 
  ones = Data[Data[,1] == 1,]
  zeros = Data[Data[,1] == 0,]
  conc=matrix(0, dim(zeros)[1], dim(ones)[1])
  disc=matrix(0, dim(zeros)[1], dim(ones)[1])
  ties=matrix(0, dim(zeros)[1], dim(ones)[1])
  for (j in 1:dim(zeros)[1])
  {
    for (i in 1:dim(ones)[1])
    {
      if (ones[i,2]>zeros[j,2])
      {conc[j,i]=1}
      else if (ones[i,2]<zeros[j,2])
      {disc[j,i]=1}
      else if (ones[i,2]==zeros[j,2])
      {ties[j,i]=1}
    }
  }
  Pairs=dim(zeros)[1]*dim(ones)[1]
  PercentConcordance=(sum(conc)/Pairs)*100
  PercentDiscordance=(sum(disc)/Pairs)*100
  PercentTied=(sum(ties)/Pairs)*100 
}

# KS Chart

fitted=concordance$x;


bin=10
score=data.frame(probability=test$LDA_Prob,response=test$Status.Reason)
score=score[order(-score$probability),]
row.names(score)=1:nrow(score)
score$bucket=floor(bin*(as.numeric(row.names(score))-1)/nrow(score))+1

library(sqldf)
Bucket_Details=sqldf(' select bucket,
                     count(*) as COUNT,
                     min(probability) as MINSCORE,
                     max(probability) as MAXSCORE,
                     (avg(probability)*100) as PREDDEFRATE,
                     sum(response) as WIN,
                     count(*)- sum(response) as LOSS
                     from score
                     group by 1')


Bucket_Details$CUM_COUNT=cumsum(Bucket_Details$COUNT)
Bucket_Details$CUM_WIN=cumsum(Bucket_Details$WIN)
Bucket_Details$WIN_CAPTRED=(Bucket_Details$WIN/sum(Bucket_Details$WIN))*100
Bucket_Details$ACTUAL_WIN_RATE=(Bucket_Details$WIN/Bucket_Details$COUNT)*100
Bucket_Details$CUM_ACTUAL_WIN=(Bucket_Details$CUM_WIN/Bucket_Details$CUM_COUNT)*100
Bucket_Details$CUM_LOSS=cumsum(Bucket_Details$LOSS)
Bucket_Details$CUM_PER_WIN=(Bucket_Details$CUM_WIN/sum(Bucket_Details$WIN))*100
Bucket_Details$CUM_PER_LOSS=(Bucket_Details$CUM_LOSS/sum(Bucket_Details$LOSS))*100
Bucket_Details$KS=Bucket_Details$CUM_PER_WIN-Bucket_Details$CUM_PER_LOSS
write.csv(Bucket_Details,file="D:/CTO/CTO wrk/Q2'17/Data Science for STrategy/trace reports/Round 3/Main modeling/ks_train_model3.csv",row.names=FALSE)

###For Test Data

test$Geo.Growth.Markets=ifelse(test$Geo=="Growth Markets",1,0)
test$Pricing.Involved=ifelse(test$Pricing.Involved=="Yes",1,0)
test$Proposal.Type.BAFO...Best.And.Final.Offer..=ifelse(test$Proposal.Type=="BAFO ( Best And Final Offer )",1,0)
test$Opportunity.Type.New=ifelse(test$Opportunity.Type=="New",1,0)
test$Add1=ifelse(test$Opportunity.Type.New<0.5,1,0)
test$Proposal.Type.Proactive.Proposal=ifelse(test$Proposal.Type=="Proactive Proposal",1,0)
test$Proposal.Type.RFI=ifelse(test$Proposal.Type=="RFI",1,0)
test$Account.Classification.Nurture=ifelse(test$Account.Classification=="Nurture",1,0)
test$SBU.Energy..Natural.Resources..Utilities..ENU.=ifelse(test$SBU=="Energy, Natural Resources, Utilities (ENU)",1,0)
test$Add3 = ifelse(test$Opportunity.Type.New>=0.5 & test$Proposal.Type.BAFO...Best.And.Final.Offer..< 0.5 & test$Proposal.Type.Proactive.Proposal< 0.5 & test$SBU.Energy..Natural.Resources..Utilities..ENU.< 0.5 & test$Proposal.Type.RFI< 0.5 & test$Account.Classification.Nurture< 0.5,1,0)



output <- predict(lr9,test,type="response")
test$Log_op=output

