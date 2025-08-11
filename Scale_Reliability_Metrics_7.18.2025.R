###Reproduces Manuscript Tables 1 & 2

rm(list=ls())

library(dplyr)   ###Version 1.1.4
library(psych)   ###Version 2.5.3

######Load in data from your local machine
Data<-read.csv("C:/Users/tfrawley/Desktop/OPC_Analysis/Survey_Data_For_Analysis_7.17.2025.csv")

###START WITH WELLBEING DATA
Data<-Data%>% mutate_at(vars('Q11._1','Q11._2', 'Q11._3', 'Q11._4', 'Q11._5', 'Q11._6', 'Q11._7', 'Q11._8', 'Q11._9'), as.numeric)

WBData<-Data[c(5:13)]

###Remove observations in instances where the question was not completely answered
WBData<-WBData[complete.cases(WBData),]
likert_questions <- WBData %>% dplyr::select(1:9)

###Find Cronbach's aplha for complete scale
cronbach.alpha(likert_questions)

###Find McDonald's omega for complete scale
omega_result<-psych::omega(likert_questions)
print(omega_result)

##Find mean score, sd score, and item-total correlation (raw.r) values
alpha_result<-psych::alpha(likert_questions)
alpha_result$item.stats

####
###MATERIAL SUB-SCALE
likert_questions <- WBData %>% dplyr::select(1,2,6,7)

###Find Cronbach's aplha for complete scale
cronbach.alpha(likert_questions)
###Find McDonald's omega for complete scale
omega_result<-psych::omega(likert_questions)
print(omega_result)
##Find mean score, sd score, and item-total correlation (raw.r) values
alpha_result<-psych::alpha(likert_questions)
alpha_result$item.stats

###Find mean sub-scale value & sd
WBData<- WBData %>% 
  mutate(MWB_Index = rowMeans(likert_questions, na.rm = TRUE))
mean(WBData$MWB_Index)
sd(WBData$MWB_Index)

###
###SUBJECTIVE SUB-SCALE
likert_questions <- WBData %>% dplyr::select(6,7,8,9)

###Find Cronbach's aplha for complete scale
cronbach.alpha(likert_questions)
###Find McDonald's omega for complete scale
omega_result<-psych::omega(likert_questions)
print(omega_result)
##Find mean score, sd score, and item-total correlation (raw.r) values
alpha_result<-psych::alpha(likert_questions)
alpha_result$item.stats

###Find mean sub-scale value & sd
WBData<- WBData %>% 
  mutate(SWB_Index = rowMeans(likert_questions, na.rm = TRUE))
mean(WBData$SWB_Index)
sd(WBData$SWB_Index)


#####
###RELATIONAL SUB-SCALE
likert_questions <- WBData %>% dplyr::select(3,4,5)

###Find Cronbach's aplha for complete scale
cronbach.alpha(likert_questions)
###Find McDonald's omega for complete scale
omega_result<-psych::omega(likert_questions)
print(omega_result)
##Find mean score, sd score, and item-total correlation (raw.r) values
alpha_result<-psych::alpha(likert_questions)
alpha_result$item.stats

###Find mean sub-scale value & sd
WBData<- WBData %>% 
  mutate(RWB_Index = rowMeans(likert_questions, na.rm = TRUE))
mean(WBData$RWB_Index)
sd(WBData$RWB_Index)

####
####
####REPEAT THE PROCESS FOR BARRIERS DATA

Data<-Data%>% mutate_at(vars('Q13._1', 'Q13._2', 'Q13._3', 'Q13._4', 'Q13._5', 'Q13._6', 'Q13._7', 'Q13._8', 'Q13._9'), as.numeric)
BData<-Data[c(14:22)]

###Remove observations in instances where the question was not completely answered
BData<-BData[complete.cases(BData),]
likert_questions <- WBData %>% dplyr::select(1:9)

###Find Cronbach's aplha for complete scale
cronbach.alpha(likert_questions)

###Find McDonald's omega for complete scale
omega_result<-psych::omega(likert_questions)
print(omega_result)

##Find mean score, sd score, and item-total correlation (raw.r) values
alpha_result<-psych::alpha(likert_questions)
alpha_result$item.stats

###
###PERSONAL BARRIERS
likert_questions <- BData %>% dplyr::select(1,3,6)

###Find Cronbach's aplha for complete scale
cronbach.alpha(likert_questions)
###Find McDonald's omega for complete scale
omega_result<-psych::omega(likert_questions)
print(omega_result)
##Find mean score, sd score, and item-total correlation (raw.r) values
alpha_result<-psych::alpha(likert_questions)
alpha_result$item.stats

###Find mean sub-scale value & sd
BData<- BData %>% 
  mutate(PB_Index = rowMeans(likert_questions, na.rm = TRUE))
mean(BData$PB_Index)
sd(BData$PB_Index)

####
####SOCIAL BARRIERS
likert_questions <- BData %>% dplyr::select(1,2,9)

###Find Cronbach's aplha for complete scale
cronbach.alpha(likert_questions)
###Find McDonald's omega for complete scale
omega_result<-psych::omega(likert_questions)
print(omega_result)
##Find mean score, sd score, and item-total correlation (raw.r) values
alpha_result<-psych::alpha(likert_questions)
alpha_result$item.stats

###Find mean sub-scale value & sd
BData<- BData %>% 
  mutate(SB_Index = rowMeans(likert_questions, na.rm = TRUE))
mean(BData$SB_Index)
sd(BData$SB_Index)

###
###KNOWLEDGE BARRIERS
likert_questions <- BData %>% dplyr::select(5,7,8)

###Find Cronbach's aplha for complete scale
cronbach.alpha(likert_questions)
###Find McDonald's omega for complete scale
omega_result<-psych::omega(likert_questions)
print(omega_result)
##Find mean score, sd score, and item-total correlation (raw.r) values
alpha_result<-psych::alpha(likert_questions)
alpha_result$item.stats

###Find mean sub-scale value & sd
BData<- BData %>% 
  mutate(KB_Index = rowMeans(likert_questions, na.rm = TRUE))
mean(BData$KB_Index)
sd(BData$KB_Index)

###
###PE BARRIERS
likert_questions <- BData %>% dplyr::select(3,4,7)

###Find Cronbach's aplha for complete scale
cronbach.alpha(likert_questions)
###Find McDonald's omega for complete scale
omega_result<-psych::omega(likert_questions)
print(omega_result)
##Find mean score, sd score, and item-total correlation (raw.r) values
alpha_result<-psych::alpha(likert_questions)
alpha_result$item.stats

###Find mean sub-scale value & sd
BData<- BData %>% 
  mutate(PEB_Index = rowMeans(likert_questions, na.rm = TRUE))
mean(BData$PEB_Index)
sd(BData$PEB_Index)

