###Reproduces Figure 6 and Supplemental Figure 5

rm(list = ls(all = TRUE))

library(dplyr)
library(pheatmap)
library(rstaix)

###Load in data from your local machine
Data<-read.csv("C:/Users/tfrawley/Desktop/OPC_Analysis/Survey_Data_For_Analysis_7.18.2025.csv")

Data<-Data%>% mutate_at(vars('Q11._1','Q11._2', 'Q11._3', 'Q11._4', 'Q11._5', 'Q11._6', 'Q11._7', 'Q11._8', 'Q11._9'), as.numeric)
Data<-Data%>% mutate_at(vars('Q13._1', 'Q13._2', 'Q13._3', 'Q13._4', 'Q13._5', 'Q13._6', 'Q13._7', 'Q13._8', 'Q13._9'), as.numeric)

###Subset to only columns of interest (Barriers Prompts, WB Prompts, & RespondentId)
Data<-Data[c(2, 5:22)]

###Only retain rows were with complete data for both the barriers questions and the wellbeing question
Data<-Data[complete.cases(Data), ]

###Find Values for Aggregate Scale and Dimensional Sub-scales
likert_questions <- Data %>% dplyr::select(2:10)
Data<- Data %>% mutate(WB_Index = rowMeans(likert_questions, na.rm = TRUE))


likert_questions <- Data %>% dplyr::select(2,3,7,8)  
Data<- Data %>% 
  mutate(Material_Index = rowMeans(likert_questions, na.rm = TRUE))

likert_questions <- Data %>% dplyr::select(7,8,9,10)
Data<- Data %>% 
  mutate(Subjective_Index = rowMeans(likert_questions, na.rm = TRUE))

likert_questions <- Data %>% dplyr::select(4,5,6)
Data<- Data %>% 
  mutate(Relational_Index = rowMeans(likert_questions, na.rm = TRUE))

likert_questions <- Data %>% dplyr::select(11:19)
Data<- Data %>% 
  mutate(Barriers_Index = rowMeans(likert_questions, na.rm = TRUE))

likert_questions <- Data %>% dplyr::select(11,12,19)
Data<- Data %>% mutate(Social_Index = rowMeans(likert_questions, na.rm = TRUE))

likert_questions <- Data %>% dplyr::select(11,13,16)
Data<- Data %>% mutate(Personal_Index = rowMeans(likert_questions, na.rm = TRUE))

likert_questions <- Data %>% dplyr::select(13,14,17)
Data<- Data %>% mutate(PE_Index = rowMeans(likert_questions, na.rm = TRUE))

likert_questions <- Data %>% dplyr::select(15,17,18)
Data<- Data %>% mutate(Knowledge_Index = rowMeans(likert_questions, na.rm = TRUE))


###Subset to only reatin scale values
Data_Sub<-Data[c(20:28)]

##Calculate correlation coefficients
X<-as.data.frame(cor(Data_Sub), method="spearman", use="pairwise.complete.obs", na.rm=TRUE)
##Subset correlation matrix to avoid looking at within barriers and within wellbeing comparisons
X<-X[c(1:4)]
X<-X[c(5:9),]
###Limit number of digits
X<-round(X,3)
##Prepare label
Label<-X

##Replace correlation values that are not significant, following the appplication of a boneferroni correction, with blank text
Y<-as.data.frame(cor_pmat(Data_Sub, method="spearman",use="pairwise.complete.obs", na.rm=TRUE))
Y<-Y[c(1:4)]
Y<-Y[c(5:9),]
Y<-Y*20
Label[Y > .01]<-" "

###Plot up correlation heatmap
pheatmap(X,
         display_numbers=Label,
         cluster_cols=FALSE)


###Repeat Process for individual prompts (see Extended Data)
Data_Sub<-Data[c(2:19)]
X<-as.data.frame(cor(Data_Sub), method="spearman", use="pairwise.complete.obs", na.rm=TRUE)
X<-X[c(1:9)]
X<-X[c(10:18),]
X<-round(X,3)
Label<-X

Y<-as.data.frame(cor_pmat(Data_Sub, method="spearman",use="pairwise.complete.obs", na.rm=TRUE))
Y<-Y[c(1:9)]
Y<-Y[c(10:18),]
Y<-Y*81

Label[Y > .01]<-" "

pheatmap(X,
         display_numbers=Label,
         cluster_cols=TRUE)


