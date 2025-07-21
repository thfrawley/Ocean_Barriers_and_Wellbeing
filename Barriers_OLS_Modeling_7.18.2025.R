###Reproduces Manuscript Figure 4


rm(list = ls(all = TRUE))

library(forcats)
library(dplyr)
library(lme4)
library(sjPlot)
library(ggplot2)


######Load in data from your local machine
Data<-read.csv("C:/Users/tfrawley/Desktop/OPC_Analysis/Survey_Data_For_Analysis_7.18.2025.csv")
Data<-Data%>% mutate_at(vars('Q13._1', 'Q13._2', 'Q13._3', 'Q13._4', 'Q13._5', 'Q13._6', 'Q13._7', 'Q13._8', 'Q13._9'), as.numeric)

###Retain only variables of interest
Data<-Data[c(14:22, 25:37)]

###Remove observations in instances where the Barriers Question was not completely answered
Data <- Data[!apply(is.na(Data[, 1:9]), 1, any), ]

likert_questions <- Data %>% dplyr::select(1:9)

Data<- Data %>% 
  mutate(B_Index = rowMeans(likert_questions, na.rm = TRUE))

Data_Subset<-Data[c(10:23)]
Data_Subset<- Data_Subset %>%
  mutate(across(c(1:10,12:13), scale))

###Remove instances where predictor data is missing to ensure all models are fitted to same number of observations
Data_Subset<-Data_Subset[complete.cases(Data_Subset),]


###Proceed with forward stepwise model selection
Model_0<-lm(B_Index~EJ_Bin, data= Data_Subset)
Model_1<-lm(B_Index~EJ_Bin+Income, data= Data_Subset)
AIC(Model_0, Model_1)

Model_1<-lm(B_Index~EJ_Bin+Income, data= Data_Subset)
Model_2<-lm(B_Index~Income, data= Data_Subset)
AIC(Model_1, Model_2)

Model_1<-lm(B_Index~EJ_Bin+Income, data= Data_Subset)
Model_3<-lm(B_Index~EJ_Bin*Income, data= Data_Subset)
AIC(Model_1, Model_3)

Model_3<-lm(B_Index~EJ_Bin*Income, data= Data_Subset)
Model_4<-lm(B_Index~EJ_Bin*Income+White+Asian+Black+Pacific_Islander+Native+Latino, data= Data_Subset)
AIC(Model_3, Model_4)

Model_4<-lm(B_Index~EJ_Bin*Income+White+Asian+Black+Pacific_Islander+Native+Latino, data= Data_Subset)
Model_5<-lmer(B_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin*Income + (1|Area), data= Data_Subset)
AIC(Model_4, Model_5)

Model_4<-lm(B_Index~EJ_Bin*Income+White+Asian+Black+Pacific_Islander+Native+Latino, data= Data_Subset)
Model_6<-lm(B_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin*Income + Area, data= Data_Subset)
AIC(Model_4, Model_6)

Model_6<-lm(B_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin*Income + Area, data= Data_Subset)
Model_7<-lm(B_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin*Income+Area+ Male+NB, data= Data_Subset)
AIC(Model_6, Model_7)

Model_7<-lm(B_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin*Income+Area+ Male+NB, data= Data_Subset)
Model_8<-lm(B_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin*Income + Male+NB+ Area + Distance, data= Data_Subset)
AIC(Model_7, Model_8)


AIC(Model_1, Model_2, Model_3, Model_4, Model_5, Model_6, Model_7, Model_8)    


##Quickly Visualize Model Estimates, and obtain significance values
plot_model(Model_7, show.values=TRUE, value.offset=0.3, value.size=3)
summary(Model_7)


##Extract & Format Data for Custom Plotting
{
summary(Model_7)
model_summary <- summary(Model_7)
X <- as.data.frame(model_summary$coefficients[, "Estimate"])
names(X)[1]<-"Estimate"
X$std_errors <- model_summary$coefficients[, "Std. Error"]
names(X)[2]<-"se"
X$Significance<- model_summary$coefficients[, "Pr(>|t|)"]
X$Significance<-ifelse(X$Significance < 0.05, "Yes", "No")
X$Variable<-row.names(X)
X <- X[-1, ]
new_row1 <- data.frame(NA,NA,NA,"Distance")
names(new_row1)<-colnames(X)
X<-rbind(X, new_row1)


X$Variable <- factor(X$Variable, levels = c("Distance", "AreaNorth", "AreaSouth",  "Male", "NB", "Asian", "Black", 'Latino', "Native" ,"Pacific_Islander", "White", "EJ_Bin", "Income", "EJ_Bin:Income"))
levels(X$Variable)[levels(X$Variable) == "NB"] <- "Trans or Non-Binary"
levels(X$Variable)[levels(X$Variable) == "Pacific_Islander"] <- "Pacific Islander"
levels(X$Variable)[levels(X$Variable) == "Income"] <- "HH Income"
levels(X$Variable)[levels(X$Variable) == "EJ_Bin"] <- "Comm. Vulnerability"
levels(X$Variable)[levels(X$Variable) == "EJ_Bin:Income"] <- "Vulnerability:Income"
X$Variable<-fct_rev(X$Variable)
X$se<-X$se*1.96
X$Direction<-ifelse(X$Estimate >0, "Positive", "Negative")
X$Label<-round(X$Estimate, 3)
All_Barriers<-X
All_Barriers$Group<-"All Barriers"
}

###Plot Publication Figure, Derive Significance Values from Previous Plot

ggplot() + geom_point(data=X, aes(x=Variable, y=Estimate, color=Direction), size=2.5) +
  geom_text(data=X, aes(x=Variable, y=Estimate, color=Direction, label=Label), size=2.5, vjust=-1) + 
  geom_errorbar(data=X, aes(x=Variable, y=Estimate, ymin=Estimate-se, ymax=Estimate+se, color=Direction), width=.02) + ylim(-.25,.15) +coord_flip() +
  theme_bw() + scale_color_manual(values=c("red3", "navyblue")) + geom_hline(yintercept = 0, color = "grey57", linetype = "dashed")


ggsave('Barrriers_Model.tif',  plot = last_plot(), dpi=300, height=5, width=5.5, units='in')

##For Personal Barriers

likert_questions <- Data %>% dplyr::select(1,3,6)

Data<- Data %>% 
  mutate(PB_Index = rowMeans(likert_questions, na.rm = TRUE))
Data_Subset<-Data[c(10:22,24)]
Data_Subset<- Data_Subset %>%
  mutate(across(c(1:10,12:13), scale))
Data_Subset<-Data_Subset[complete.cases(Data_Subset), ]


Model_0<-lm(PB_Index~EJ_Bin, data= Data_Subset)
Model_1<-lm(PB_Index~EJ_Bin+Income, data= Data_Subset)
Model_2<-lm(PB_Index~Income, data= Data_Subset)
Model_3<-lm(PB_Index~EJ_Bin*Income, data= Data_Subset)
Model_4<-lm(PB_Index~EJ_Bin*Income+White+Asian+Black+Pacific_Islander+Native+Latino, data= Data_Subset)
Model_5<-lmer(PB_Index~EJ_Bin*Income+White+Asian+Black+Pacific_Islander+Native+Latino + (1|Area), data= Data_Subset)
Model_6<-lm(PB_Index~EJ_Bin*Income+White+Asian+Black+Pacific_Islander+Native+Latino + Area, data= Data_Subset)
Model_7<-lm(PB_Index~EJ_Bin*Income+White+Asian+Black+Pacific_Islander+Native+Latino + Area + Male + NB, data= Data_Subset)
Model_8<-lm(PB_Index~EJ_Bin*Income+White+Asian+Black+Pacific_Islander+Native+Latino + Area + Male + NB + Distance, data= Data_Subset)

AIC(Model_0, Model_1, Model_2, Model_3, Model_4, Model_5, Model_6, Model_7, Model_8)


##Quickly Visualize Model Estimates, and obtain significance values
plot_model(Model_7, show.values=TRUE, value.offset=0.3, value.size=3)
summary(Model_7)

##Extract & Format Data for Custom Plotting
{
model_summary <- summary(Model_7)
X <- as.data.frame(model_summary$coefficients[, "Estimate"])
names(X)[1]<-"Estimate"
X$std_errors <- model_summary$coefficients[, "Std. Error"]
names(X)[2]<-"se"
X$Significance<- model_summary$coefficients[, "Pr(>|t|)"]
X$Significance<-ifelse(X$Significance < 0.05, "Yes", "No")
X$Variable<-row.names(X)
X <- X[-1, ]
new_row1 <- data.frame(NA,NA,NA,"Distance")
names(new_row1)<-colnames(X)


X$Variable <- factor(X$Variable, levels = c("Distance", "AreaNorth", "AreaSouth",  "Male", "NB", "Asian", "Black", 'Latino', "Native" ,"Pacific_Islander", "White", "EJ_Bin", "Income", "EJ_Bin:Income"))
levels(X$Variable)[levels(X$Variable) == "NB"] <- "Trans or Non-Binary"
levels(X$Variable)[levels(X$Variable) == "Pacific_Islander"] <- "Pacific Islander"
levels(X$Variable)[levels(X$Variable) == "Income"] <- "HH Income"
levels(X$Variable)[levels(X$Variable) == "EJ_Bin"] <- "Comm. Vulnerability"
levels(X$Variable)[levels(X$Variable) == "EJ_Bin:Income"] <- "Vulnerability:Income"
X$Variable<-fct_rev(X$Variable)
X$se<-X$se*1.96
X$Direction<-ifelse(X$Estimate >0, "Positive", "Negative")
X$Label<-round(X$Estimate, 3)
Personal_Barriers<-X
Personal_Barriers$Group<-"Personal"
}

##For Social Barriers

likert_questions <- Data %>% dplyr::select(1,2,9)

Data<- Data %>% 
  mutate(SB_Index = rowMeans(likert_questions, na.rm = TRUE))
Data_Subset<-Data[c(10:22,25)]
Data_Subset<- Data_Subset %>%
  mutate(across(c(1:10,12:13), scale))
Data_Subset<-Data_Subset[complete.cases(Data_Subset), ]

Model_0<-lm(SB_Index~EJ_Bin, data= Data_Subset)
Model_1<-lm(SB_Index~EJ_Bin+Income, data= Data_Subset)
Model_2<-lm(SB_Index~Income, data= Data_Subset)
Model_3<-lm(SB_Index~EJ_Bin+White+Asian+Black+Pacific_Islander+Native+Latino, data= Data_Subset)
Model_4<-lmer(SB_Index~EJ_Bin+White+Asian+Black+Pacific_Islander+Native+Latino + (1|Area), data= Data_Subset)
Model_5<-lm(SB_Index~EJ_Bin+White+Asian+Black+Pacific_Islander+Native+Latino + Area, data= Data_Subset)
Model_6<-lm(SB_Index~EJ_Bin+White+Asian+Black+Pacific_Islander+Native+Latino + Area + Male + NB, data= Data_Subset)
Model_7<-lm(SB_Index~EJ_Bin+White+Asian+Black+Pacific_Islander+Native+Latino + Area + Male + NB + Distance, data= Data_Subset)

AIC(Model_0, Model_1, Model_2, Model_3, Model_4, Model_5, Model_6, Model_7)

##Quickly Visualize Model Estimates, and obtain significance values
plot_model(Model_6, show.values=TRUE, value.offset=0.3, value.size=3)
summary(Model_6)

##Extract & Format Data for Custom Plotting
{
model_summary <- summary(Model_6)
X <- as.data.frame(model_summary$coefficients[, "Estimate"])
names(X)[1]<-"Estimate"
X$std_errors <- model_summary$coefficients[, "Std. Error"]
names(X)[2]<-"se"
X$Significance<- model_summary$coefficients[, "Pr(>|t|)"]
X$Significance<-ifelse(X$Significance < 0.05, "Yes", "No")
X$Variable<-row.names(X)
X <- X[-1, ]
new_row1 <- data.frame(NA,NA,NA,"EJ_Bin")
names(new_row1)<-colnames(X)
new_row2 <- data.frame(NA,NA,NA,"Income")
names(new_row2)<-colnames(X)
new_row3 <- data.frame(NA,NA,NA,"Distance")
names(new_row3)<-colnames(X)
new_row4 <- data.frame(NA,NA,NA,"EJ_Bin:Income")
names(new_row4)<-colnames(X)


X<-rbind(X, new_row1)
X<-rbind(X, new_row2)
X<-rbind(X, new_row3)
X<-rbind(X, new_row4)

X$Variable <- factor(X$Variable, levels = c("Distance", "AreaNorth", "AreaSouth",  "Male", "NB", "Asian", "Black", 'Latino', "Native" ,"Pacific_Islander", "White", "EJ_Bin", "Income", "EJ_Bin:Income"))
levels(X$Variable)[levels(X$Variable) == "NB"] <- "Trans or Non-Binary"
levels(X$Variable)[levels(X$Variable) == "Pacific_Islander"] <- "Pacific Islander"
levels(X$Variable)[levels(X$Variable) == "Income"] <- "HH Income"
levels(X$Variable)[levels(X$Variable) == "EJ_Bin"] <- "Comm. Vulnerability"
levels(X$Variable)[levels(X$Variable) == "EJ_Bin:Income"] <- "Vulnerability:Income"
X$Variable<-fct_rev(X$Variable)
X$se<-X$se*1.96
X$Direction<-ifelse(X$Estimate >0, "Positive", "Negative")
X$Label<-round(X$Estimate, 3)
Social_Barriers<-X
Social_Barriers$Group<-"Social"
}

##For Knowledge Barriers

likert_questions <- Data %>% dplyr::select(5,7,8)

Data<- Data %>% 
  mutate(KB_Index = rowMeans(likert_questions, na.rm = TRUE))
Data_Subset<-Data[c(10:22,26)]
Data_Subset<- Data_Subset %>%
  mutate(across(c(1:10,12:13), scale))
Data_Subset<-Data_Subset[complete.cases(Data_Subset), ]

Model_0<-lm(KB_Index~EJ_Bin, data= Data_Subset)
Model_1<-lm(KB_Index~EJ_Bin+Income, data= Data_Subset)
Model_2<-lm(KB_Index~Income, data= Data_Subset)
Model_3<-lm(KB_Index~EJ_Bin*Income, data= Data_Subset)
Model_4<-lm(KB_Index~EJ_Bin*Income+White+Asian+Black+Pacific_Islander+Native+Latino, data= Data_Subset)
Model_5<-lmer(KB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin*Income + (1|Area), data= Data_Subset)
Model_6<-lm(KB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin*Income + Area, data= Data_Subset)
Model_7<-lm(KB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin*Income +Area + Male + NB, data= Data_Subset)
Model_8<-lm(KB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin*Income + Area+Male+NB+ Distance, data= Data_Subset)

AIC(Model_0, Model_1, Model_2, Model_3, Model_4, Model_5, Model_6, Model_7, Model_8)

##Quickly Visualize Model Estimates, and obtain significance values
plot_model(Model_7, show.values=TRUE, value.offset=0.3, value.size=3)
summary(Model_7)

##Extract & Format Data for Custom Plotting
{
model_summary <- summary(Model_7)
X <- as.data.frame(model_summary$coefficients[, "Estimate"])
names(X)[1]<-"Estimate"
X$std_errors <- model_summary$coefficients[, "Std. Error"]
names(X)[2]<-"se"
X$Significance<- model_summary$coefficients[, "Pr(>|t|)"]
X$Significance<-ifelse(X$Significance < 0.05, "Yes", "No")
X$Variable<-row.names(X)
X <- X[-1, ]
new_row1 <- data.frame(NA,NA,NA,"Distance")
names(new_row1)<-colnames(X)

X<-rbind(X, new_row1)



X$Variable <- factor(X$Variable, levels = c("Distance", "AreaNorth", "AreaSouth",  "Male", "NB", "Asian", "Black", 'Latino', "Native" ,"Pacific_Islander", "White", "EJ_Bin", "Income", "EJ_Bin:Income"))
levels(X$Variable)[levels(X$Variable) == "NB"] <- "Trans or Non-Binary"
levels(X$Variable)[levels(X$Variable) == "Pacific_Islander"] <- "Pacific Islander"
levels(X$Variable)[levels(X$Variable) == "Income"] <- "HH Income"
levels(X$Variable)[levels(X$Variable) == "EJ_Bin"] <- "Comm. Vulnerability"
levels(X$Variable)[levels(X$Variable) == "EJ_Bin:Income"] <- "Vulnerability:Income"
X$Variable<-fct_rev(X$Variable)
X$se<-X$se*1.96
X$Direction<-ifelse(X$Estimate >0, "Positive", "Negative")
X$Label<-round(X$Estimate, 3)
Knowledge_Barriers<-X
Knowledge_Barriers$Group<-"Knowledge"
}

####
##For PE_Barriers

likert_questions <- Data %>% dplyr::select(3,4,7)

Data<- Data %>% 
  mutate(PEB_Index = rowMeans(likert_questions, na.rm = TRUE))
Data_Subset<-Data[c(10:22,27)]
Data_Subset<- Data_Subset %>%
  mutate(across(c(1:10,12:13), scale))
Data_Subset<-Data_Subset[complete.cases(Data_Subset), ]


Model_0<-lm(PEB_Index~EJ_Bin, data= Data_Subset)
Model_1<-lm(PEB_Index~EJ_Bin+Income, data= Data_Subset)
Model_2<-lm(PEB_Index~Income, data= Data_Subset)
Model_3<-lm(PEB_Index~EJ_Bin*Income, data= Data_Subset)
Model_4<-lm(PEB_Index~EJ_Bin*Income+White+Asian+Black+Pacific_Islander+Native+Latino, data= Data_Subset)
Model_5<-lmer(PEB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin*Income + (1|Area), data= Data_Subset)
Model_6<-lm(PEB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin*Income + Area, data= Data_Subset)
Model_7<-lm(PEB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin*Income +Area + Male + NB, data= Data_Subset)
Model_8<-lm(PEB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin*Income + Area+Male+NB+ Distance, data= Data_Subset)

AIC(Model_0,Model_1, Model_2, Model_3, Model_4, Model_5, Model_6, Model_7, Model_8)

##Quickly Visualize Model Estimates, and obtain significance values
plot_model(Model_8, show.values=TRUE, value.offset=0.3, value.size=3)
summary(Model_8)

##Extract & Format Data for Custom Plotting
{
model_summary <- summary(Model_8)
X <- as.data.frame(model_summary$coefficients[, "Estimate"])
names(X)[1]<-"Estimate"
X$std_errors <- model_summary$coefficients[, "Std. Error"]
names(X)[2]<-"se"
X$Significance<- model_summary$coefficients[, "Pr(>|t|)"]
X$Significance<-ifelse(X$Significance < 0.05, "Yes", "No")
X$Variable<-row.names(X)
X <- X[-1, ]

X$Variable <- factor(X$Variable, levels = c("Distance", "AreaNorth", "AreaSouth",  "Male", "NB", "Asian", "Black", 'Latino', "Native" ,"Pacific_Islander", "White", "EJ_Bin", "Income", "EJ_Bin:Income"))
levels(X$Variable)[levels(X$Variable) == "NB"] <- "Trans or Non-Binary"
levels(X$Variable)[levels(X$Variable) == "Pacific_Islander"] <- "Pacific Islander"
levels(X$Variable)[levels(X$Variable) == "Income"] <- "HH Income"
levels(X$Variable)[levels(X$Variable) == "EJ_Bin"] <- "Comm. Vulnerability"
levels(X$Variable)[levels(X$Variable) == "EJ_Bin:Income"] <- "Vulnerability:Income"
X$Variable<-fct_rev(X$Variable)
X$se<-X$se*1.96
X$Direction<-ifelse(X$Estimate >0, "Positive", "Negative")
X$Label<-round(X$Estimate, 3)
PE_Barriers<-X
PE_Barriers$Group<-"Physical-Environ."
}


### Assemble SUB-Scale COMPARISON PLOT


Aggregate<-rbind(Personal_Barriers, Knowledge_Barriers, Social_Barriers, PE_Barriers)


ggplot() +
  geom_point(data=Aggregate, aes(x=Variable, y=Estimate, color=Group, shape=Significance), size=2, position =  position_dodge(width = 0.75))+
  geom_errorbar(data=Aggregate, aes(y=Estimate, x=Variable, ymin=Estimate-se, ymax=Estimate+se, color=Group, lty=Significance), width=0, position = position_dodge(width = 0.75)) + 
  scale_color_manual(values=c( "goldenrod2",  "mediumseagreen", "steelblue3", "hotpink3")) + theme_bw() + scale_shape_manual(values=c(1,16)) + scale_linetype_manual(values=c('dashed', 'solid')) +
  coord_flip()+ geom_hline(yintercept = 0, color = "grey57", linetype = "dashed")
 
ggsave('Barrriers_Model_Types.tif',  plot = last_plot(), dpi=300, height=5, width=6, units='in')



