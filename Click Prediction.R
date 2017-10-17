rm(list = ls())
gc()

setwd("F:\\Mach. Learning\\Kaggle case studies\\Click Prediction - AV Hackathon")

library(ggplot2)
library(data.table)

dt = fread("train.csv")
test = fread("test.csv")
train_bkp = copy(train)
test_bkp = copy(test)


train_miss = sapply(train,function(x) sum(is.na(x)) + length(which(x == "")))
tr_miss_per = round(train_miss/nrow(train),4)
test_miss = sapply(test,function(x) sum(is.na(x)) + length(which(x == "")))
te_miss_per = round(test_miss/nrow(test),4)

df = data.frame(tr_miss_per)
df$VariableName = rownames(df)
ggplot(df,aes(VariableName,tr_miss_per,fill = VariableName))+
  geom_bar(stat = "identity")+
  theme(legend.position = "None",axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Missing Percentage in Train")
ggsave("Train - Missing.png")

df = data.frame(te_miss_per)
df$VariableName = rownames(df)
ggplot(df,aes(VariableName,te_miss_per,fill = VariableName))+
  geom_bar(stat = "identity")+
  theme(legend.position = "None",axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Missing Percentage in Test")
ggsave("Test - Missing.png")

### Funciton for returning mode...
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

woe<- function(data,Ind,Dep){
	levels = unique(data[[Ind]])
	final = data.table()
	for(i in levels){
		bin = i
		good = nrow(data[data[[Ind]] == i & data[[Dep]] == 0,])
		bad = nrow(data[data[[Ind]] == i & data[[Dep]] == 1,])
		total = good + bad
		f = data.table(Bin = bin,Good = good, Bad = bad, Total = total)
		final = rbind(final,f)
	}	
	final[,GoodPerc := Good/sum(Good)]
	final[,BadPerc := Bad/sum(Bad)]
	final[,WOE := log(GoodPerc/BadPerc)*100]
	return(final)
}
## ID is unique..
dt[,ID := NULL]

## Country has "**" as one of the values .. Replacing that also with mode...
dt[,Country := ifelse(Country %in% c("**",""),"Others",Country)]  ## Filling with Mode 'IN'
country_names = dt[,sum(ConversionStatus),Country][V1 == 0,Country]
dt[,Country := ifelse(Country %in% country_names,"No Event",Country)]
country_woe = woe(dt,"Country","ConversionStatus")
dt[,Country := ifelse(Country %in% c("MA","BJ"),"IN",Country)]
dt[,Country := ifelse(Country %in% c("CD","IT","UK","SA"),"ES",Country)]
dt[,Country := ifelse(Country %in% c("DZ","IR","HT"),"MX",Country)]
dt[,Country := ifelse(Country %in% c("IQ","CL"),"BR",Country)]
dt[,Country := ifelse(Country %in% c("EC","DE","PL","TW"),"AE",Country)]
dt[,Country := ifelse(Country %in% c("PT","JP","CN","BG","GR","UA","JO","AF","TG"),"US",Country)]
dt[,Country := ifelse(Country %in% c("SY","CH","RO"),"SG",Country)]
dt[,Country := ifelse(Country %in% c("MY","TN","ZM","CR"),"Others",Country)]
dt[,Country := ifelse(Country %in% c("MM","PE","JM"),"CO",Country)]
dt[,Country := ifelse(Country %in% c("LY","UY","EG","QA","TZ","KH","OM","FI"),"FR",Country)]
dt[,Country := ifelse(Country %in% c("VE","AL"),"KR",Country)]
dt[,Country := ifelse(Country %in% c("PY","RS","BH","AU","LB","DK","BY","PS"),"BO",Country)]
dt[,Country := ifelse(Country %in% c("CA","LT","CM","HU"),"RW",Country)]
dt[,Country := ifelse(Country %in% c("SR","NI"),"PK",Country)]
dt[,Country := ifelse(Country %in% c("MG","VN","CG","HR","UG"),"RU",Country)]
dt[,Country := ifelse(Country %in% c("CI","AZ","KZ","PA","MZ","HN"),"PH",Country)]
dt[,Country := ifelse(Country %in% c("BN","BD"),"KW",Country)]
dt[,Country := ifelse(Country %in% c("SN","ZA"),"NG",Country)]
dt[,Country := ifelse(Country %in% c("MW","LK","GN","SV"),"IE",Country)]
dt[,Country := ifelse(Country %in% c("GA","MV","ML","NP","KE","SD"),"AR",Country)]
dt[,Country := ifelse(Country %in% c("MQ"),"YT",Country)]

dt[,Country := as.factor(Country)]

## Carrier has no missing values...
carrier_names = dt[,sum(ConversionStatus),Carrier][V1 == 0,Carrier]
dt[,Carrier := ifelse(Carrier %in% carrier_names,"9999",Carrier)]
carrier_woe = woe(dt,"Carrier","ConversionStatus")

dt[,Carrier := as.integer(as.character(Carrier))]

dt[,Carrier := ifelse(Carrier %in% c(53,205,2,246,654),3,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(341,218,52,49,45),6,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(337,43,22,159,502,984,-1,289,194),136,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(562,55,18,340,377,518),56,Carrier)]
gc()
dt[,Carrier := ifelse(Carrier %in% c(172),967,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(12,130,425),74,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(503,278,653,352),1,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(137,292,500,679,5,801,650,193),4,Carrier)]
gc()
dt[,Carrier := ifelse(Carrier %in% c(516),26,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(667,916),724,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(270,979,725),975,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(71,375,525,670,990,808),7,Carrier)]
gc()
dt[,Carrier := ifelse(Carrier %in% c(574,836),320,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(57,121,79,339,991,260),669,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(268,317,34),42,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(9),24,Carrier)]
gc()
dt[,Carrier := ifelse(Carrier %in% c(371,648),32,Carrier)]

dt[,Carrier := ifelse(Carrier %in% c(134,427,276,597,977,142),430,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(703,284,123,39,655,652,712,336,20,124,25),64,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(982,973,103,41,985,27,565,66,54,950,132,131),704,Carrier)]
gc()
dt[,Carrier := ifelse(Carrier %in% c(603,78,215,986,677,498,818,277,231),428,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(125,210,15,266),47,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(964,681,531,361,48),355,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(338,618,217,699,46,133,176,86),523,Carrier)]
gc()
dt[,Carrier := ifelse(Carrier %in% c(226,50,456,17),294,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(592,85,13,602,612),14,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(814,593),381,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(530,44,290),174,Carrier)]
gc()
dt[,Carrier := ifelse(Carrier %in% c(448,429,733,485),40,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(58,269),418,Carrier)]
dt[,Carrier := ifelse(Carrier %in% c(771,641),731,Carrier)]
gc()
dt[is.na(Carrier),Carrier := 9999]
dt[,Carrier := as.factor(as.character(Carrier))]

sub = dt[,.(Country,Carrier,TrafficType,Browser,OS,ConversionStatus,ClickDate)]
write.csv(sub,"Cleaned Data v2.csv",row.names = F)
sub = fread("Cleaned Data v2.csv")
ones = sub[ConversionStatus == TRUE]
zeros = sub[ConversionStatus == FALSE]
ind = sample(1:nrow(zeros),80556,replace = F)
zeros = zeros[ind,]

data = rbind(zeros,ones)
data[,ConversionStatus := as.integer(ConversionStatus)]
data[,ConversionStatus := as.factor(ConversionStatus)]

data[,Country := as.factor(Country)]
data[,Carrier := as.factor(Carrier)]
data[,TrafficType := as.factor(TrafficType)]
data[,Browser := as.factor(Browser)]
data[,OS := as.factor(OS)]

# data[,ClickDate := as.POSIXct(ClickDate,"%Y-%m-%d %H:%M:%S")]
# data[,WeekDay := weekdays(as.Date(format(ClickDate,"%Y-%m-%d")))]
# data[,WeekDay := as.factor(WeekDay)]
# data[,HourOfDay := as.factor(format(ClickDate,"%H"))]
# data[,Weekend := as.factor(ifelse(WeekDay %in% c("Sunday","Saturday"),"Yes","No"))]

data1 = copy(data)
data[,ClickDate := NULL]
ind = createDataPartition(data$ConversionStatus , p = 0.70, list = F)
train = data[ind,]
valid = data[-ind,]

fit = glm(ConversionStatus ~ ., data = train, family = binomial)
p = predict(fit,valid,type = "response")
table(p>0.5,valid$ConversionStatus)

train[,ConversionStatus := ifelse(ConversionStatus == "1","Yes","No")]
train[,ConversionStatus := as.factor(ConversionStatus)]
trainControl = trainControl(method = "cv",number = 5,savePredictions = "final",classProbs = TRUE)
models = caretList(ConversionStatus ~ .,data = train,methodList = c("rpart","glm","knn"),trControl = trainControl)

greedy_ensemble <- caretEnsemble(
  models 
  )

DTFit = train(ConversionStatus ~ .,data = train,method = "rpart",trControl = trainControl)

library(randomForest)
rf = randomForest(ConversionStatus ~ ., data = data)

## Browser has missing values ..
dt[,Browser := ifelse(Browser == "","Others",Browser)]
browser_woe = woe(dt,"Browser","ConversionStatus")
dt[,Browser := ifelse(Browser %in% c("dorado","mqq_browser","wap_browser","netfront","firefox","phantom","novarra_nweb","edge_mobile","webkit/android","teleca-obigo","maui_wap_browser","nokia_browser","openwave_mobile_browser","opera_tablet","microsoft_mobile_explorer","presto/opera_mobi","gecko/minimo","webkit/webos","netfront_nx","ovibrowser_(nokia_s40)","polaris","semc"),"No Event",Browser)]
dt[,Browser := ifelse(Browser == "firefox_mobile","46.0.2490.76",ifelse(Browser %in% c("chromium","tizen_browser"),"android_webkit",ifelse(Browser == "iemobile","samsung",Browser)))]

dt[,Browser := as.factor(Browser)]

## OS has missing values .. Filling with mode...
dt[,OS := ifelse(OS == "","Others",OS)]
os_woe = woe(dt,"OS","ConversionStatus")
dt[,OS := ifelse(OS %in% c("Mac OS X","Xbox OS","PlayStation OS","Firefox OS","RIM Tablet OS","Windows RT","MTK/Nucleus OS","Bada OS","MeeGo","Windows Mobile OS","webOS","Linux Smartphone OS","PSP OS"),"No Event",OS)]
dt[,OS := ifelse(OS %in% c("Tizen"),"Android",OS)]
dt[,OS := ifelse(OS == "RIM OS","Windows Phone OS",OS)]
dt[,OS := as.factor(OS)]


## TrafficType has missing values.. Filling with mode as of now..
dt[,TrafficType := ifelse(TrafficType == "","Others",TrafficType)]
traffic_woe = woe(dt,"TrafficType","ConversionStatus")
dt[,TrafficType := as.factor(TrafficType)]

## Device has missing values.. Filling with mode..
dt[,Device := ifelse(Device == "","Generic",Device)]
dt[,Device := as.factor(Device)]
device_names = dt[,sum(ConversionStatus),Device][V1 == 0,Device]



dt[,ClickDate := as.POSIXct(ClickDate,"%Y-%m-%d %H:%M:%S")]
dt[,WeekDay := weekdays(as.Date(format(ClickDate,"%Y-%m-%d")))]
dt[,HourOfDay := as.factor(format(ClickDate,"%H"))]
dt[,Weekend := as.factor(ifelse(WeekDay %in% c("Sunday","Saturday"),"Yes","No"))]


data = dt[ConversionStatus == TRUE]
data[,(rem) := NULL]
data[,ClickDate := as.POSIXct(ClickDate,"%Y-%m-%d %H:%M:%S")]
data[,WeekDay := weekdays(as.Date(format(ClickDate,"%Y-%m-%d")))]
data[,HourOfDay := as.factor(format(ClickDate,"%H"))]
data[,Weekend := as.factor(ifelse(WeekDay %in% c("Sunday","Saturday"),"Yes","No"))]
## Removing features with more than 70% missing values

rem = c("ID","RefererUrl","subPublisherId","ConversionStatus","UserIp","publisherId","advertiserCampaignId")
train[,(rem) := NULL]

dt[,ConversionStatus := as.integer(ConversionStatus)]
dt[,ConversionStatus := as.factor(as.character(ConversionStatus))]


library(caret)
library(Matrix)
library(xgboost)


ind = createDataPartition(train$ConversionStatus,p = 0.80, list = F)
train1 = train[ind,]
valid1 = train[-ind,]

train[,ConversionStatus := as.integer(ConversionStatus)-1]
valid[,ConversionStatus := as.integer(ConversionStatus)-1]
dtrain = xgb.DMatrix(data = sparse.model.matrix(ConversionStatus~.-1,data=train),label=train$ConversionStatus)
dvalid = xgb.DMatrix(data = sparse.model.matrix(ConversionStatus~.-1,data=valid),label=valid$ConversionStatus)


set.seed(10)
xgb.c = xgb.cv(data = dtrain,
				nfold = 5,
                 nrounds =300, 
                 max_depth = 5,
                 objective = "binary:logistic",
                 lambda = 5,
                 subsample = 1,
                 colsample_bytree = 1,
                 watchlist = list(eval=dtest,train = dtrain),
                 print.every.n = 10)

xgb <- xgb.train(data = dtrain,
                 nrounds =282, 
                 max_depth = 5,
                 objective = "binary:logistic",
                 lambda = 5,
                 subsample = 1,
                 colsample_bytree = 1,
                 watchlist = list(eval=dvalid,train = dtrain),
                 print.every.n = 10
)
p = predict(xgb,dvalid)

### Regression

reg_data = train[ConversionStatus == TRUE]

reg[,Country := as.factor(as.character(Country))]

reg[,TrafficType := ifelse(TrafficType == "","A",TrafficType)]
reg[,TrafficType := as.factor(TrafficType)]

reg[,Carrier := as.factor(as.character(Carrier))]

#reg[,ClickDate := as.POSIXct(ClickDate,"%Y-%m-%m %H:%M:%S")]


library(randomForest)
rf = randomForest(ConversionPayOut ~ .,data = train)



### Test data - Preprocessing...

test[,Country := as.character(Country)]
test[,Country := ifelse(Country %in% c("","**"),"Others",Country)]
test[,Country := ifelse(Country %in% country_names,"No Event",Country)]
test[,Country := ifelse(Country %in% c("MA","BJ"),"IN",Country)]
test[,Country := ifelse(Country %in% c("CD","IT","UK","SA"),"ES",Country)]
test[,Country := ifelse(Country %in% c("DZ","IR","HT"),"MX",Country)]
test[,Country := ifelse(Country %in% c("IQ","CL"),"BR",Country)]

gc()

test[,Country := ifelse(Country %in% c("EC","DE","PL","TW"),"AE",Country)]
test[,Country := ifelse(Country %in% c("PT","JP","CN","BG","GR","UA","JO","AF","TG"),"US",Country)]
test[,Country := ifelse(Country %in% c("SY","CH","RO"),"SG",Country)]
test[,Country := ifelse(Country %in% c("MY","TN","ZM","CR"),"Others",Country)]
test[,Country := ifelse(Country %in% c("MM","PE","JM"),"CO",Country)]

gc()

test[,Country := ifelse(Country %in% c("LY","UY","EG","QA","TZ","KH","OM","FI"),"FR",Country)]
test[,Country := ifelse(Country %in% c("VE","AL"),"KR",Country)]
test[,Country := ifelse(Country %in% c("PY","RS","BH","AU","LB","DK","BY","PS"),"BO",Country)]
test[,Country := ifelse(Country %in% c("CA","LT","CM","HU"),"RW",Country)]
test[,Country := ifelse(Country %in% c("SR","NI"),"PK",Country)]

gc()

test[,Country := ifelse(Country %in% c("MG","VN","CG","HR","UG"),"RU",Country)]
test[,Country := ifelse(Country %in% c("CI","AZ","KZ","PA","MZ","HN"),"PH",Country)]
test[,Country := ifelse(Country %in% c("BN","BD"),"KW",Country)]
test[,Country := ifelse(Country %in% c("SN","ZA"),"NG",Country)]
test[,Country := ifelse(Country %in% c("MW","LK","GN","SV"),"IE",Country)]

gc()

test[,Country := ifelse(Country %in% c("GA","MV","ML","NP","KE","SD"),"AR",Country)]
test[,Country := ifelse(Country %in% c("MQ"),"YT",Country)]

test[,Country := as.factor(Country)]

test[,Carrier := as.character(Carrier)]
carrier_names = as.character(carrier_names)
test[,Carrier := ifelse(Carrier %in% carrier_names,"9999",Carrier)]

test[,Carrier := as.integer(Carrier)]
test[,Carrier := ifelse(Carrier %in% c(53,205,2,246,654),3,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(341,218,52,49,45),6,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(337,43,22,159,502,984,-1,289,194),136,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(562,55,18,340,377,518),56,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(172),967,Carrier)]
gc()
test[,Carrier := ifelse(Carrier %in% c(12,130,425),74,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(503,278,653,352),1,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(137,292,500,679,5,801,650,193),4,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(516),26,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(667,916),724,Carrier)]
gc()
test[,Carrier := ifelse(Carrier %in% c(270,979,725),975,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(71,375,525,670,990,808),7,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(574,836),320,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(57,121,79,339,991,260),669,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(268,317,34),42,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(9),24,Carrier)]
gc()
test[,Carrier := ifelse(Carrier %in% c(371,648),32,Carrier)]

test[,Carrier := ifelse(Carrier %in% c(134,427,276,597,977,142),430,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(703,284,123,39,655,652,712,336,20,124,25),64,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(982,973,103,41,985,27,565,66,54,950,132,131),704,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(603,78,215,986,677,498,818,277,231),428,Carrier)]
gc()
test[,Carrier := ifelse(Carrier %in% c(125,210,15,266),47,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(964,681,531,361,48),355,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(338,618,217,699,46,133,176,86),523,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(226,50,456,17),294,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(592,85,13,602,612),14,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(814,593),381,Carrier)]
gc()
test[,Carrier := ifelse(Carrier %in% c(530,44,290),174,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(448,429,733,485),40,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(58,269),418,Carrier)]
test[,Carrier := ifelse(Carrier %in% c(771,641),731,Carrier)]
gc()
test[is.na(Carrier),Carrier := 9999]
test[,Carrier := as.factor(as.character(Carrier))]

test[,Browser := ifelse(Browser == "","Others",Browser)]
test[,Browser := ifelse(Browser %in% c("dorado","mqq_browser","wap_browser","netfront","firefox","phantom","novarra_nweb","edge_mobile","webkit/android","teleca-obigo","maui_wap_browser","nokia_browser","openwave_mobile_browser","opera_tablet","microsoft_mobile_explorer","presto/opera_mobi","gecko/minimo","webkit/webos","netfront_nx","ovibrowser_(nokia_s40)","polaris","semc"),"No Event",Browser)]
test[,Browser := ifelse(Browser == "firefox_mobile","46.0.2490.76",ifelse(Browser %in% c("chromium","tizen_browser"),"android_webkit",ifelse(Browser == "iemobile","samsung",Browser)))]

test[,Browser := as.factor(Browser)]


test[,OS := ifelse(OS == "","Others",OS)]
test[,OS := ifelse(OS %in% c("Mac OS X","Xbox OS","PlayStation OS","Firefox OS","RIM Tablet OS","Windows RT","MTK/Nucleus OS","Bada OS","MeeGo","Windows Mobile OS","webOS","Linux Smartphone OS","PSP OS"),"No Event",OS)]
test[,OS := ifelse(OS %in% c("Tizen"),"Android",OS)]
test[,OS := ifelse(OS == "RIM OS","Windows Phone OS",OS)]
test[,OS := as.factor(OS)]


## TrafficType has missing values.. Filling with mode as of now..
test[,TrafficType := ifelse(TrafficType == "","Others",TrafficType)]
test[,TrafficType := as.factor(TrafficType)]

test[,ClickDate := as.POSIXct(ClickDate,"%Y-%m-%d %H:%M:%S")]
test[,WeekDay := weekdays(as.Date(format(ClickDate,"%Y-%m-%d")))]
test[,HourOfDay := as.factor(format(ClickDate,"%H"))]
test[,Weekend := as.factor(ifelse(WeekDay %in% c("Sunday","Saturday"),"Yes","No"))]

test1 = test[,.(Country,Carrier,TrafficType,Browser,OS)]
dtest = xgb.DMatrix(data = sparse.model.matrix(~.-1,data=test1))

p = predict(xgb,dtest)

test[,ConversionStatus := ifelse(p>0.95,1,0)]

s1 = test[ConversionStatus == 1]

test[,ConversionPayOut := ifelse(ConversionStatus == 1,5,0)]
write.csv(test[,.(ID,ConversionPayOut)],"Submission3.csv",row.names = F)


reg[,Country := as.factor(Country)]
reg[,Carrier := as.factor(Carrier)]
reg[,TrafficType := as.factor(TrafficType)]
reg[,Browser := as.factor(Browser)]
reg[,OS := as.factor(OS)]


reg = sub[ConversionStatus == TRUE]
reg[,ConversionStatus := NULL]
reg[,ClickDate := NULL]

ind = sample(1:nrow(reg), round(0.7 * nrow(reg)) , replace = F)
train = reg[ind,]
valid = reg[-ind,]
train = train[!is.na(ConversionPayOut)]
valid = valid[!is.na(ConversionPayOut)]
dtrain = xgb.DMatrix(data = sparse.model.matrix(ConversionPayOut~.-1,data=train),label=train$ConversionPayOut)
dvalid = xgb.DMatrix(data = sparse.model.matrix(ConversionPayOut~.-1,data=valid),label=valid$ConversionPayOut)


set.seed(10)
xgb1 <- xgb.train(data = dtrain,
                 nrounds =200, 
                 max_depth = 2,
                 objective = "reg:linear",
                 subsample = 1,
                 colsample_bytree = 1,
                 watchlist = list(train = dtrain,eval=dvalid),
                 print.every.n = 10,
)


ppp = test[ConversionStatus == 1]
t1 = ppp[,.(Country,Carrier,TrafficType,Browser,OS)]
dtest = xgb.DMatrix(data = sparse.model.matrix(~.-1,data=t1))


rf = randomForest(ConversionPayOut~.,data = train)

t1 = s1[,.(Country,Carrier,TrafficType,Browser,OS)]
dtest = xgb.DMatrix(data = sparse.model.matrix(~.-1,data=t1))

pp = predict(xgb1,dtest)