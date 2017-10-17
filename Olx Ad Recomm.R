
### Challenge 1

f <- file("stdin")
on.exit(close(f))
T <- readLines(f)
n = as.integer(T[1])
library(stringr)
prod_table = data.frame()
S_Prod_Cat_ind = grepl()
for(i in 1:n){
	q = T[i+1]
	split = unlist(str_split(q," "))
	if(split[1] == "S"){
		if(grepl("\\.",split[3])){
			P_ID = as.integer(unlist(str_split(split[3],"\\."))[1])
			C_ID = as.integer(unlist(str_split(split[3],"\\."))[2])
		}
		else{
			P_ID = as.integer(split[3])
			C_ID = 0
		}
		if(grepl("\\.",split[4])){
			S_ID = as.integer(unlist(str_split(split[4],"\\."))[1])
			R_ID = as.integer(unlist(str_split(split[4],"\\."))[2])
		}
		else{
			S_ID = as.integer(split[4])
			R_ID = 0
		}
		prod_table = rbind(prod_table,data.frame(Day = split[2],P_ID = P_ID,C_ID = C_ID,S_ID = S_ID,R_ID = R_ID))
	}
	else{
		if(grepl("\\.",split[2])){
			d_strt = as.integer(unlist(str_split(split[2],"\\."))[1])
			d_end = as.integer(unlist(str_split(split[2],"\\."))[2])
		}
		else{
			d_strt = as.integer(split[2])
			d_end = as.integer(split[2])
		}
		t = prod_table[prod_table$Day %in% c(d_strt:d_end),]
		if(grepl("\\.",split[3])){
			P_ID = as.integer(unlist(str_split(split[3],"\\."))[1])
			C_ID = as.integer(unlist(str_split(split[3],"\\."))[2])
			t = t[t$P_ID == P_ID & t$C_ID == C_ID,]
		}
		else{
			P_ID = as.integer(split[3])
			if(P_ID != -1){
					t = t[t$P_ID == P_ID,]
			}
		}
		if(grepl("\\.",split[4])){
			S_ID = as.integer(unlist(str_split(split[4],"\\."))[1])
			R_ID =as.integer(unlist(str_split(split[4],"\\."))[2])
			t = t[t$S_ID == S_ID & t$R_ID == R_ID,]

		}
		else{
			S_ID = as.integer(split[4])
			if(S_ID != -1){
					t = t[t$S_ID == S_ID,]
			} 
		}
		
	write(nrow(t),stdout())
	}
}
#### Optimized code...

f <- file("stdin")
on.exit(close(f))
T <- readLines(f)
n = as.integer(T[1])
library(stringr)
library(data.table)
subset = T[-1]
base_table = data.table(do.call(rbind,lapply(subset,function(x) unlist(strsplit(x," ")))))
split_1 = ifelse(base_table[,V1]=="S",TRUE,FALSE)
length = sum(split_1)
split_2 = grepl("\\.",base_table[,V2])
split_3 = grepl("\\.",base_table[,V3])
split_4 = grepl("\\.",base_table[,V4])

base_table[,sp2 := as.integer(split_2)]
base_table[,sp3 := as.integer(split_3)]
base_table[,sp4 := as.integer(split_4)]
P_val = integer(length(split_3))
C_val = integer(length(split_3))
P_val[split_3 == TRUE] = sapply(base_table[sp3==1,V3],function(x) as.integer(unlist(str_split(x,"\\."))[1]))
C_val[split_3 == TRUE] = sapply(base_table[sp3==1,V3],function(x) as.integer(unlist(str_split(x,"\\."))[2]))
S_val = integer(length(split_4))
R_val = integer(length(split_4))
S_val[split_4 == TRUE] = sapply(base_table[sp4==1,V4],function(x) as.integer(unlist(str_split(x,"\\."))[1]))
R_val[split_4 == TRUE] = sapply(base_table[sp4==1,V4],function(x) as.integer(unlist(str_split(x,"\\."))[2]))
Start_val = integer(length(split_2))
End_val = integer(length(split_2))
Start_val[split_2 == TRUE] = sapply(base_table[sp2==1,V2],function(x) as.integer(unlist(str_split(x,"\\."))[1]))
End_val[split_2 == TRUE] = sapply(base_table[sp2==1,V2],function(x) as.integer(unlist(str_split(x,"\\."))[2]))



prod_table = data.table(Day=integer(length),P_ID = integer(length),C_ID = integer(length),S_ID = integer(length),R_ID = integer(length),row = 1:length)
k = 1
for(i in 1:n){
	q = subset[i]
	split = unlist(str_split(q," "))
	if(split_1[i]){
			PC = split_3[i]
			P = ifelse(PC,P_val[i],as.integer(split[3]))
			C = ifelse(PC,C_val[i],0)
			SR = split_4[i]
			S = ifelse(SR,S_val[i],as.integer(split[4]))
			R = ifelse(SR,R_val[i],0)
			prod_table[k,':='(Day = as.integer(split[2]),P_ID = P,C_ID = C,S_ID = S,R_ID = R)]
			k = k + 1
	}
	else{

			st_end = split_2[i]
			d_strt =ifelse(st_end,Start_val[i],as.integer(split[2]))
			d_end = ifelse(st_end,End_val[i],as.integer(split[2]))
			day_ind = which(prod_table$Day %in% c(d_strt:d_end))
			if(split_3[i]){
				P = P_val[i]
				C = C_val[i]
				ind = which(prod_table$row %in% day_ind & prod_table$P_ID == P & prod_table$C_ID == C)
			}else{
				P = as.integer(split[3])
				if(P != -1){
						ind = which(prod_table$row %in% day_ind & prod_table$P_ID == P)
				}
			}
			if(split_4[i]){
				S = S_val[i]
				R = R_val[i]
				ind = which(prod_table$row %in% ind & prod_table$S_ID == S & prod_table$R_ID == R)

			}else{
				S = as.integer(split[4])
				if(S != -1){
						ind = which(prod_table$row %in% ind & prod_table$S_ID == S)
				} 
			}
		
	write(length(ind),stdout())
	ind = integer()
	}
}



### Challenge 2 - OLX Ad recommendation...

rm(list = ls())
setwd("F:\\Mach. Learning\\Kaggle case studies\\OLX Ad Recommendation")

user_data = fread("user_data.csv")
ads = fread("ads_data.csv")

user_msgs = fread("user_messages.csv")
usr_msg_txt = fread("user_messages_test.csv")

sapply(user_data,function(x) sum(is.na(x)) + length(which(x=="")))
sapply(ads,function(x) sum(is.na(x)) + length(which(x=="")))
sapply(user_msgs,function(x) sum(is.na(x)) + length(which(x=="")))
sapply(usr_msg_txt,function(x) sum(is.na(x)) + length(which(x=="")))

m = merge(user_data,ads,by = c("ad_id"))
m[,description := NULL]