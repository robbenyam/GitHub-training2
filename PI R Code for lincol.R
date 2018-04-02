rm(list = ls())

#setwd("/home/dbmanager/Documents/test")
setwd("C:/Users/bocheng.ren/Documents/PRO/PI_for_lincol")

#install new packages for this project
pkg <- c("openxlsx","stringr", "zoo", "ggplot2", "quantmod", "readr","lars","glmnet","tidyverse")
new_pkg <- pkg[(!(pkg %in% installed.packages()[, "Package"])) |
                 (pkg %in% old.packages()[, "Package"])]
for (p in new_pkg){
  install.packages(p)
  cat("installed package:",p)
}

#set needed package for project
library(openxlsx)
library(zoo)
library(lubridate)
library(quantmod)
library(stringr)
library(tidyverse)
library(glmnet)
library(MASS)
library(Matrix)
library(dplyr)

#########################################################################
#create lags
#Weifan: Define number of lags 
NLAG = 55

#initial set
#REGTYPE 1=regression 2=ridge 3=lasso
REGTYPE = 3
#number of regression test
NREGRESSION = 5000
#sample size for var
NREGRESSOR = 20
#sample size for sample
NSAMPLE = 200
#Moving Averages frequency
MA = 30#
#select target var to remove
DEL = c("D1","D10","D14","M1","M10","M14")

#########################################################################
#file input
#Weifan: Change input data file and start/end data

message("start process")

#Raw = read.xlsx("Raw_data_for_lincol.xlsx", sheet = "Input")
#read raw-data from file and change the format
Raw = read.xlsx("Raw_data_for_lincol.xlsx", sheet = "Input")
Raw$Day = as.Date(Raw$Day, origin = "1899-12-30")
names(Raw)[names(Raw) == 'Day'] <- 'TimeID'
names(Raw)[names(Raw) == 'Sales'] <- 'DepVar'

#delete visit variable but why?
#Raw <- Raw %>% select (-c(D1,M1))  # delete 'visit' variable
#Raw <- within(Raw,rm(D1,M1))
Raw <- within(Raw,rm(D1,D10,D14,M1,M10,M14))
names(Raw)

#set time span i need to adjust this for lincole
Start = as.Date("1/1/2016","%m/%d/%Y")
End = as.Date("2/28/2018","%m/%d/%Y")

#no idea about this work, do i need to do this for lincole?
#add_var = 'V10'  # new variable for V10 (remaining days of this month) , V11 (last work day of this month)

#remove data out of time span
Clean = subset(Raw, Raw$TimeID >= Start & Raw$TimeID <= End)
str(Clean)
write.csv(Raw, file = paste0("Raw_Data_",Start,"_",End,".csv"))

#########################################################################
#process time
#add new time items
#find out every the first day for each week , month and year

message("time format change process")

TimeDictionary = data.frame(TimeID = seq(Start, End, by = "day"))
TimeDictionary$Month_NUM = format(TimeDictionary$TimeID , "%m")
TimeDictionary$Year_NUM = format(TimeDictionary$TimeID , "%Y")
TimeDictionary$Day_StartDate = TimeDictionary$TimeID 
TimeDictionary$Month_StartDate = as.Date(with(TimeDictionary, paste(Year_NUM, Month_NUM, 1 ,sep="-")), "%Y-%m-%d")
TimeDictionary$Year_StartDate = as.Date(with(TimeDictionary, paste(Year_NUM, 1, 1, sep="-")), "%Y-%m-%d")
TimeDictionary$Week_StartDate = ifelse(
wday(TimeDictionary$TimeID ) == 1, 
TimeDictionary$TimeID - 6, 
TimeDictionary$TimeID -wday(TimeDictionary$TimeID )+2) 
TimeDictionary$Week_StartDate  =  as.Date(TimeDictionary$Week_StartDate , format = "%Y-%m-%d")
TimeDictionary$Month_NUM = NULL
TimeDictionary$Year_NUM = NULL

#########################################################################
#create clean data

message("create clean data")

Clean = merge(Clean, TimeDictionary, by = "TimeID")
Clean$TimeID = NULL
NUMS = sapply(Clean , is.numeric) #check data type of list

#grouping clean data by day , week ,month and year and caculate the sum for each list
Clean_Day = aggregate(Clean[, NUMS], by = list(Clean$Day_StartDate), FUN=sum)
names(Clean_Day)[names(Clean_Day) == "Group.1"] = "StartDate"
Clean_Week = aggregate(Clean[, NUMS], by = list(Clean$Week_StartDate), FUN=sum)
names(Clean_Week)[names(Clean_Week) == "Group.1"] = "StartDate"
Clean_Month = aggregate(Clean[, NUMS], by = list(Clean$Month_StartDate), FUN=sum)
names(Clean_Month)[names(Clean_Month) == "Group.1"] = "StartDate"
Clean_Year = aggregate(Clean[, NUMS], by = list(Clean$Year_StartDate), FUN=sum)
names(Clean_Year)[names(Clean_Year) == "Group.1"] = "StartDate"

Clean = Clean_Day

#########################################################################
#create lags
#Weifan: Define number of lags 
#55 = 8 weeks (7*8-1)
#lags is not needed here

message("crate lags")

if (NLAG == 0){
  LagData= Clean
} else {
  LagTemp = Clean
  LagTemp$StartDate = NULL
  LagTemp$DepVar= NULL
  LagTemp = data.frame(LagTemp)

  i = 1
  repeat{
  if(i > ncol(LagTemp))
  {break}
  message(i)
  Temp = NULL
  Temp = Lag(zoo(LagTemp[,i]),k=1:NLAG)
  for(n in 1:NLAG)
  {
    LaggedVarName = paste(noquote(colnames(LagTemp)[i]), paste("LAG", n, sep = ""), sep = "_")
    names(Temp)[n] = paste(LaggedVarName)
  }
  Temp = cbind(data.frame(LagTemp[,i]), data.frame(Temp))
  names(Temp)[1] = paste(noquote(colnames(LagTemp)[i]))
  if(i == 1){
    LagData = data.frame(Temp)
  } else {
    LagData = cbind(data.frame(LagData), data.frame(Temp))
  }
  i = i + 1
  }

  LagData = cbind(Clean$StartDate, Clean$DepVar, LagData)
  names(LagData)[1] = "StartDate"
  names(LagData)[2] = "DepVar"
}
  write.csv(LagData, file = paste0("LagData_1_",Start,"_",End,".csv"))

##
#  if (add_var == 'V10'){
#    LagData$V10 <- -(LagData$StartDate %>% ceiling_date('month') - days(1) - LagData$StartDate)
#  } else if (add_var == 'V11'){
#    eod <- LagData$StartDate %>% ceiling_date('month') - days(1)
#    for (i in 1:length(eod)){
#   if (wday(eod[i]) == 1){
#      eod[i] <- eod[i] - days(2)
#     } else if (wday(eod[i]) == 7){
#        eod[i] <- eod[i] - days(1)
#      } else {eod[i]
#      }
#    }
#    LagData$V11 <- apply(LagData$StartDate %>% as.matrix(),1,function(x){
#      if (x %in% eod){1}else {0}
#    })
#  }
#}
##
#########################################################################
#random regressions
#Weifan: change number of regressions and number of regressors

message("regression start")

#choose one cost function from three types
if(REGTYPE==1){
  message("regre")
}else if(REGTYPE==2){
  message("ridge")
}else{
  message("lasso")
}

i = 1
repeat{
if(i > NREGRESSION)
{break}

#stepwise
CoeffTemp = NULL
#Random selection of varables
RandomVars = sample(LagData[-1:-2], NREGRESSOR, replace=FALSE)

#Random selection of sample
#RandomVars = sample_n(LagData[-1:-2], NSAMPLE, replace=FALSE)
#write.csv(RandomVars, file = paste0("RandomVars_",Start,"_",End,".csv"))

#calculate moving average
if (MA == 0){
  RandomVars = cbind(data.frame(Clean$DepVar), data.frame(RandomVars))
} else {
  RandomVars = cbind(data.frame(SMA(Clean$DepVar, MA)), data.frame(RandomVars))
}

names(RandomVars)[1] = "DepVar"
write.csv(RandomVars, file = paste0("RandomVars_BMA_",Start,"_",End,".csv"))
RandomVars = na.omit(RandomVars)

if (REGTYPE == 1){
  TempReg = step(lm(DepVar ~ ., data = RandomVars), direction="both", trace=0)
}else if(REGTYPE == 2){
  TempReg = lm.ridge(DepVar ~ ., data = RandomVars,lambda = 0)
}else{
  Y <- as.matrix(RandomVars$DepVar)
  X <- as.matrix(RandomVars[-1])
  TempReg = glmnet(X,Y,alpha = 1,lambda = 0)    
}

if (REGTYPE==1){
 CoeffTemp = data.frame(TempReg$coefficients)
}else if(REGTYPE==2){
 CoeffTemp = data.frame(TempReg$coef)  
}else{
 CoeffTemp = data.frame(as.matrix(TempReg$beta))
}
#write.csv(CoeffTemp, file = paste0("CoeffTemp_",Start,"_",End,".csv"))
CoeffTemp$Var = row.names(CoeffTemp)
row.names(CoeffTemp)= NULL
names(CoeffTemp)[1] = "Value"
CoeffTemp$ID = i
#value:Coefficients for each var
#var:names of var
#ID:Round number of process

#step 2, only positive stepwise
# CoeffTemp = TempReg$coefficients %>% as.data.frame() 
# CoeffTemp$Var = row.names(CoeffTemp)
# names(CoeffTemp)[1] = "value"
# CoeffTemp = CoeffTemp %>% filter(value>0) %>% filter(Var != "(Intercept)") 
# 
# RandomVars = RandomVars %>% select(DepVar, one_of(CoeffTemp$Var)) %>% as.data.frame()
# RandomVars = na.omit(RandomVars)
# 
# TempReg = step(lm(DepVar ~ ., data = RandomVars), direction="backward", trace=0)
# RSQ = summary(TempReg)$r.squared 
# T = summary(TempReg)[["coefficients"]][, "t value"]
# CoeffTemp = data.frame(cbind(TempReg$coefficients, T, RSQ, i))
# CoeffTemp$Var = row.names(CoeffTemp)
# row.names(CoeffTemp)= NULL
# names(CoeffTemp) = c("value", "t-stats", "rsq", "model_num", "var")

# TempPred = predict(TempReg)
# TempPred = as.data.frame(TempPred)
# TempPred = as.data.frame(cbind(paste("Pred", i, sep = "_"), TempPred, RSQ, as.data.frame(row.names(TempPred))))
# row.names(TempPred)= NULL
# names(TempPred) = c("model_num", "pred", "rsq", "obs_num")

	if(i == 1){
   	CoeffSummary = CoeffTemp
	#PredSummary = TempPred
	} else {
	CoeffSummary = rbind(data.frame(CoeffSummary), data.frame(CoeffTemp))
	 #PredSummary = rbind(data.frame(PredSummary), data.frame(TempPred))
	}
message(i)
i = i + 1
}

# MIN_RSQ = 0.3
# RSQ_DIS = CoeffSummary %>% group_by(model_num) %>% summarise(rsq = mean(rsq))
# summary(RSQ_DIS$rsq)
# hist(RSQ_DIS$rsq)
# # CoeffSummary = CoeffSummary %>% filter(rsq >= MIN_RSQ)
# head(CoeffSummary)

message("create final files")
write.csv(CoeffSummary, file = paste0("CoeffSummaryTemp_",Start,"_",End,".csv"))

CoeffSummary = data.frame(CoeffSummary)
CoeffSummary$Temp = str_split_fixed(CoeffSummary$Var, "_", 2)
CoeffSummary$VarName = CoeffSummary$Temp[,1]
CoeffSummary$Lag = CoeffSummary$Temp[,2]
CoeffSummary$Lag = as.numeric(gsub("LAG","",CoeffSummary$Lag))
CoeffSummary$Temp = NULL
head(CoeffSummary)

# Raw = Clean[-1:-2]
# if (MA == 0){
#   Raw = cbind(Clean$StartDate, data.frame(Clean$DepVar), data.frame(Raw))
# } else {
#   Raw = cbind(Clean$StartDate, data.frame(SMA(Clean$DepVar,MA)), data.frame(Raw))
# }
# 
# names(Raw)[1] = "StartDate"
# names(Raw)[2] = "DepVar"
#Raw= na.omit(Raw) # missing value after MA

# PredSummary = PredSummary %>% filter(rsq >= MIN_RSQ) %>% group_by(obs_num) %>%
# summarize(pred = mean(pred)) %>% as.data.frame()
# PredSummary$obs_num = as.character(PredSummary$obs_num)
# PredSummary$obs_num = as.numeric(PredSummary$obs_num)
# PredSummary = PredSummary %>% arrange(obs_num)
# Raw$obs_num = as.numeric(row.names(Raw))
# PredSummary = left_join(Raw, PredSummary, by = "obs_num") %>% select(StartDate, DepVar, pred)
# Raw$obs_num = NULL
# Raw$V10 <- LagData$V10

# write.csv(Raw, file = "Raw.csv")
write.csv(CoeffSummary, file = "CoeffRaw.csv")
#write.csv(PredSummary, file = "Pred.csv")
