###Reproduces Manuscript Figure 5

rm(list = ls(all = TRUE))

library(forcats)  ###Version 1.0.0
library(dplyr)    ###Version 1.1.4
library(lme4)     ###Version 1.1-37
library(sjPlot)   ###Version 2.9.0
library(ggplot2)  ###Version 3.5.1


######Load in data from your local machine
Data<-read.csv("C:/Users/tfrawley/Desktop/OPC_Analysis/Survey_Data_For_Analysis_12.19.2025.csv")
Data<-Data%>% mutate_at(vars('Q11._1','Q11._2', 'Q11._3', 'Q11._4', 'Q11._5', 'Q11._6', 'Q11._7', 'Q11._8', 'Q11._9'), as.numeric)

###Retain only variables of interest
Data<-Data[c(5:13, 25:37)]

###Remove observations in instances where the Barriers Question was not completely answered
Data <- Data[!apply(is.na(Data[, 1:9]), 1, any), ]


likert_questions <- Data %>% dplyr::select(1:9)


Data<- Data %>% 
  mutate(WB_Index = rowMeans(likert_questions, na.rm = TRUE))

Data_Subset<-Data[c(10:23)]
Data_Subset<- Data_Subset %>%
  mutate(across(c(1:10,12:13), scale))

###Remove instances where predictor data is missing to ensure all models are fitted to same number of observations
Data_Subset<-Data_Subset[complete.cases(Data_Subset),]


###Proceed with forward stepwise model selection
Model_0<-lm(WB_Index~EJ_Bin, data= Data_Subset)
Model_1<-lm(WB_Index~EJ_Bin+Income, data= Data_Subset)
AIC(Model_0, Model_1)

Model_0<-lm(WB_Index~EJ_Bin, data= Data_Subset)
Model_2<-lm(WB_Index~Income, data= Data_Subset)
AIC(Model_0, Model_2)

Model_0<-lm(WB_Index~EJ_Bin, data= Data_Subset)
Model_3<-lm(WB_Index~EJ_Bin*Income, data= Data_Subset)
AIC(Model_0, Model_3)

Model_0<-lm(WB_Index~EJ_Bin, data= Data_Subset)
Model_4<-lm(WB_Index~EJ_Bin+White+Asian+Black+Pacific_Islander+Native+Latino, data= Data_Subset)
AIC(Model_0, Model_4)

Model_4<-lm(WB_Index~EJ_Bin+White+Asian+Black+Pacific_Islander+Native+Latino, data= Data_Subset)
Model_5<-lmer(WB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin*Income + (1|Area), data= Data_Subset)
AIC(Model_4, Model_5)

Model_4<-lm(WB_Index~EJ_Bin+White+Asian+Black+Pacific_Islander+Native+Latino, data= Data_Subset)
Model_6<-lm(WB_Index~EJ_Bin+White+Asian+Black+Pacific_Islander+Native+Latino + Area, data= Data_Subset)
AIC(Model_4, Model_6)

Model_6<-lm(WB_Index~EJ_Bin+White+Asian+Black+Pacific_Islander+Native+Latino + Area, data= Data_Subset)
Model_7<-lm(WB_Index~EJ_Bin+White+Asian+Black+Pacific_Islander+Native+Latino +Area+ Male+NB, data= Data_Subset)
AIC(Model_6, Model_7)

Model_6<-lm(WB_Index~EJ_Bin+White+Asian+Black+Pacific_Islander+Native+Latino + Area, data= Data_Subset)
Model_8<-lm(WB_Index~EJ_Bin+White+Asian+Black+Pacific_Islander+Native+Latino + Area + Distance, data= Data_Subset)
AIC(Model_6, Model_8)


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
new_row1 <- data.frame(NA,NA,NA,"Male")
names(new_row1)<-colnames(X)
new_row2 <- data.frame(NA,NA,NA,"NB")
names(new_row2)<-colnames(X)
new_row3 <- data.frame(NA,NA,NA,"Income")
names(new_row3)<-colnames(X)
X<-rbind(X, new_row1)
X<-rbind(X, new_row2)

X$Variable <- factor(X$Variable, levels = c("Distance", "AreaNorth", "AreaSouth",  "Male", "NB", "Asian", "Black", 'Latino', "Native" ,"Pacific_Islander", "White", "EJ_Bin", "Income"))
levels(X$Variable)[levels(X$Variable) == "NB"] <- "Trans or Non-Binary"
levels(X$Variable)[levels(X$Variable) == "Pacific_Islander"] <- "Pacific Islander"
levels(X$Variable)[levels(X$Variable) == "Income"] <- "HH Income"
levels(X$Variable)[levels(X$Variable) == "EJ_Bin"] <- "Comm. Vulnerability"
levels(X$Variable)[levels(X$Variable) == "Income:EJ_Bin"] <- "Income x CES Score"
X$Variable<-fct_rev(X$Variable)
X$se<-X$se*1.96
X$Direction<-ifelse(X$Estimate >0, "Positive", "Negative")
X$Label<-round(X$Estimate, 3)
All_WB<-X
All_WB$Group<-"All WB"
}

ggplot() + geom_point(data=X, aes(x=Variable, y=Estimate, color=Direction), size=2.5) +
  geom_text(data=X, aes(x=Variable, y=Estimate, color=Direction, label=Label), size=2.5, vjust=-1) + 
  geom_errorbar(data=X, aes(x=Variable, y=Estimate, ymin=Estimate-se, ymax=Estimate+se, color=Direction), width=.02) + ylim(-.15,.15) +coord_flip() +
  theme_bw() + scale_color_manual(values=c("red3", "navyblue")) + geom_hline(yintercept = 0, color = "grey57", linetype = "dashed")


ggsave('WB_Model.tif',  plot = last_plot(), dpi=300, height=5, width=5.5, units='in')


##For Material WB

likert_questions <- Data %>% dplyr::select(1,2,6,7)

Data<- Data %>% 
  mutate(MWB_Index = rowMeans(likert_questions, na.rm = TRUE))

Data_Subset<-Data[c(10:22,24)]
Data_Subset<- Data_Subset %>%
  mutate(across(c(1:10,12:13), scale))
Data_Subset<-Data_Subset[complete.cases(Data_Subset), ]


Model_0<-lm(MWB_Index~EJ_Bin, data= Data_Subset)
Model_1<-lm(MWB_Index~EJ_Bin+Income, data= Data_Subset)
Model_2<-lm(MWB_Index~Income, data= Data_Subset)
Model_3<-lm(MWB_Index~EJ_Bin*Income, data= Data_Subset)
Model_4<-lm(MWB_Index~EJ_Bin+White+Asian+Black+Pacific_Islander+Native+Latino, data= Data_Subset)
Model_5<-lmer(MWB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin + (1|Area), data= Data_Subset)
Model_6<-lm(MWB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin + Area, data= Data_Subset)
Model_7<-lm(MWB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin + Area + Male + NB, data= Data_Subset)
Model_8<-lm(MWB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin + Area + Male + NB + Distance, data= Data_Subset)


AIC(Model_0, Model_1, Model_2, Model_3, Model_4, Model_5, Model_6, Model_7, Model_8)


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
new_row1 <- data.frame(NA,NA,NA,"Income")
names(new_row1)<-colnames(X)


X$Variable <- factor(X$Variable, levels = c("Distance", "AreaNorth", "AreaSouth",  "Male", "NB", "Asian", "Black", 'Latino', "Native" ,"Pacific_Islander", "White", "EJ_Bin", "Income"))
levels(X$Variable)[levels(X$Variable) == "NB"] <- "Trans or Non-Binary"
levels(X$Variable)[levels(X$Variable) == "Pacific_Islander"] <- "Pacific Islander"
levels(X$Variable)[levels(X$Variable) == "Income"] <- "HH Income"
levels(X$Variable)[levels(X$Variable) == "EJ_Bin"] <- "Comm. Vulnerability"
levels(X$Variable)[levels(X$Variable) == "Income:EJ_Bin"] <- "Income x CES Score"
X$Variable<-fct_rev(X$Variable)
X$se<-X$se*1.96
X$Direction<-ifelse(X$Estimate >0, "Positive", "Negative")
X$Label<-round(X$Estimate, 3)
Material_WB<-X
Material_WB$Group<-"Material"
}



##For Subjective Wellbeing

likert_questions <- Data %>% dplyr::select(6,7,8,9)

Data<- Data %>% 
  mutate(SWB_Index = rowMeans(likert_questions, na.rm = TRUE))
Data_Subset<-Data[c(10:22,25)]
Data_Subset<- Data_Subset %>%
  mutate(across(c(1:10,12:13), scale))
Data_Subset<-Data_Subset[complete.cases(Data_Subset), ]


Model_0<-lm(SWB_Index~EJ_Bin, data= Data_Subset)
Model_1<-lm(SWB_Index~EJ_Bin+Income, data= Data_Subset)
Model_2<-lm(SWB_Index~Income, data= Data_Subset)
Model_3<-lm(SWB_Index~EJ_Bin*Income, data= Data_Subset)
Model_4<-lm(SWB_Index~EJ_Bin+White+Asian+Black+Pacific_Islander+Native+Latino, data= Data_Subset)
Model_5<-lmer(SWB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin + (1|Area), data= Data_Subset)
Model_6<-lm(SWB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin + Area, data= Data_Subset)
Model_7<-lm(SWB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin +Male + NB, data= Data_Subset)
Model_8<-lm(SWB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin + Distance, data= Data_Subset)

AIC(Model_0, Model_1, Model_2, Model_3, Model_4, Model_5, Model_6, Model_7, Model_8)

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
new_row1 <- data.frame(NA,NA,NA,"Male")
names(new_row1)<-colnames(X)
new_row2 <- data.frame(NA,NA,NA,"NB")
names(new_row2)<-colnames(X)
new_row3 <- data.frame(NA,NA,NA,"Income")
names(new_row3)<-colnames(X)
new_row4 <- data.frame(NA,NA,NA,"AreaNorth")
names(new_row4)<-colnames(X)
new_row5 <- data.frame(NA,NA,NA,"AreaSouth")
names(new_row5)<-colnames(X)

X<-rbind(X, new_row1)
X<-rbind(X, new_row2)
X<-rbind(X, new_row4)
X<-rbind(X, new_row5)


X$Variable <- factor(X$Variable, levels = c("Distance", "AreaNorth", "AreaSouth",  "Male", "NB", "Asian", "Black", 'Latino', "Native" ,"Pacific_Islander", "White", "EJ_Bin", "Income"))
levels(X$Variable)[levels(X$Variable) == "NB"] <- "Trans or Non-Binary"
levels(X$Variable)[levels(X$Variable) == "Pacific_Islander"] <- "Pacific Islander"
levels(X$Variable)[levels(X$Variable) == "Income"] <- "HH Income"
levels(X$Variable)[levels(X$Variable) == "EJ_Bin"] <- "Comm. Vulnerability"
levels(X$Variable)[levels(X$Variable) == "Income:EJ_Bin"] <- "Income x CES Score"
X$Variable<-fct_rev(X$Variable)
X$se<-X$se*1.96
X$Direction<-ifelse(X$Estimate >0, "Positive", "Negative")
X$Label<-round(X$Estimate, 3)
Subjective_WB<-X
Subjective_WB$Group<-"Subjective"
}

##For Relational Wellbeing

likert_questions <- Data %>% dplyr::select(3,4,5)


Data<- Data %>% 
  mutate(RWB_Index = rowMeans(likert_questions, na.rm = TRUE))
Data_Subset<-Data[c(10:22,26)]
Data_Subset<- Data_Subset %>%
  mutate(across(c(1:10,12:13), scale))
Data_Subset<-Data_Subset[complete.cases(Data_Subset), ]

Model_0<-lm(RWB_Index~EJ_Bin, data= Data_Subset)
Model_1<-lm(RWB_Index~EJ_Bin+Income, data= Data_Subset)
Model_2<-lm(RWB_Index~Income, data= Data_Subset)
Model_3<-lm(RWB_Index~EJ_Bin*Income, data= Data_Subset)
Model_4<-lm(RWB_Index~EJ_Bin+White+Asian+Black+Pacific_Islander+Native+Latino, data= Data_Subset)
Model_5<-lmer(RWB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin + (1|Area), data= Data_Subset)
Model_6<-lm(RWB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin + Area, data= Data_Subset)
Model_7<-lm(RWB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin +Male + NB, data= Data_Subset)
Model_8<-lm(RWB_Index~White+Asian+Black+Pacific_Islander+Native+Latino + EJ_Bin + Distance, data= Data_Subset)

AIC(Model_0, Model_1, Model_2, Model_3, Model_4, Model_6, Model_7, Model_8)

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
new_row1 <- data.frame(NA,NA,NA,"Male")
names(new_row1)<-colnames(X)
new_row2 <- data.frame(NA,NA,NA,"NB")
names(new_row2)<-colnames(X)
new_row3 <- data.frame(NA,NA,NA,"Income")
names(new_row3)<-colnames(X)
new_row4 <- data.frame(NA,NA,NA,"AreaNorth")
names(new_row4)<-colnames(X)
new_row5 <- data.frame(NA,NA,NA,"AreaSouth")
names(new_row5)<-colnames(X)

X<-rbind(X, new_row1)
X<-rbind(X, new_row2)
X<-rbind(X, new_row4)
X<-rbind(X, new_row5)


X$Variable <- factor(X$Variable, levels = c("Distance", "AreaNorth", "AreaSouth",  "Male", "NB", "Asian", "Black", 'Latino', "Native" ,"Pacific_Islander", "White", "EJ_Bin", "Income"))
levels(X$Variable)[levels(X$Variable) == "NB"] <- "Trans or Non-Binary"
levels(X$Variable)[levels(X$Variable) == "Pacific_Islander"] <- "Pacific Islander"
levels(X$Variable)[levels(X$Variable) == "Income"] <- "HH Income"
levels(X$Variable)[levels(X$Variable) == "EJ_Bin"] <- "Comm. Vulnerability"
levels(X$Variable)[levels(X$Variable) == "Income:EJ_Bin"] <- "Income x CES Score"
X$Variable<-fct_rev(X$Variable)
X$se<-X$se*1.96
X$Direction<-ifelse(X$Estimate >0, "Positive", "Negative")
X$Label<-round(X$Estimate, 3)
Relational_WB<-X
Relational_WB$Group<-"Relational"
}

### Assemble SUB-Scale COMPARISON PLOT

Aggregate<-rbind(Material_WB, Subjective_WB, Relational_WB)

ggplot() +
  geom_point(data=Aggregate, aes(x=Variable, y=Estimate, color=Group, shape=Significance), size=2, position =  position_dodge(width = 0.75))+
  geom_errorbar(data=Aggregate, aes(y=Estimate, x=Variable, ymin=Estimate-se, ymax=Estimate+se, color=Group, lty=Significance), width=0, position = position_dodge(width = 0.75)) + 
  scale_color_manual(values=c( "#1B9E77",  "#D95F02", "#7570B3")) + theme_bw() + scale_shape_manual(values=c(1,16)) + scale_linetype_manual(values=c('dashed', 'solid')) +
  ##geom_point(data=All_Barriers, aes(x=Variable, y=Estimate, shape=Significance), size=3) + coord_flip()  + 
  coord_flip()+ geom_hline(yintercept = 0, color = "grey57", linetype = "dashed")

ggsave('Barrriers_Model_Types.tif',  plot = last_plot(), dpi=300, height=5, width=5.5, units='in')





