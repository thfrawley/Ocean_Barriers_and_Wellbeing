###Reproduces values and analysis presented manuscript Figures 4 & 6 and Supplemental Tables 4 & 5.

rm(list=ls())

library(dplyr)
library(psych)
library(ggplot2)
library(forcats)
library(ltm)


######Load in data from your local machine
Data<-read.csv("C:/Users/tfrawley/Desktop/OPC_Project/OPC_Analysis/Survey_Data_For_Analysis_12.17.2025.csv")

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

##Find mean score, sd score, and item-total correlation (raw.r) values for Individual Prompts
alpha_result_complete<-psych::alpha(likert_questions)
alpha_result_complete$item.stats


####REPEAT THE PROCESS FOR THE SUB-SCALES

###MATERIAL SUB-SCALE
material_likert_questions <- WBData %>% dplyr::select(1,2,6,7)

###Find Cronbach's aplha 
cronbach.alpha(material_likert_questions)
###Find McDonald's omega for complete scale
omega_result<-psych::omega(material_likert_questions)
print(omega_result)
##Find mean score, sd score, and item-total correlation (raw.r) values
alpha_result<-psych::alpha(material_likert_questions)
alpha_result$item.stats

###Find mean sub-scale value & sd
WBData<- WBData %>% 
  mutate(MWB_Index = rowMeans(material_likert_questions, na.rm = TRUE))
mean(WBData$MWB_Index)
sd(WBData$MWB_Index)

###
###SUBJECTIVE SUB-SCALE
subjective_likert_questions <- WBData %>% dplyr::select(6,7,8,9)

###Find Cronbach's aplha for complete scale
cronbach.alpha(subjective_likert_questions)
###Find McDonald's omega for complete scale
omega_result<-psych::omega(subjective_likert_questions)
print(omega_result)
##Find mean score, sd score, and item-total correlation (raw.r) values
alpha_result<-psych::alpha(subjective_likert_questions)
alpha_result$item.stats

###Find mean sub-scale value & sd
WBData<- WBData %>% 
  mutate(SWB_Index = rowMeans(subjective_likert_questions, na.rm = TRUE))
mean(WBData$SWB_Index)
sd(WBData$SWB_Index)


#####
###RELATIONAL SUB-SCALE
relational_likert_questions <- WBData %>% dplyr::select(3,4,5)

###Find Cronbach's aplha for complete scale
cronbach.alpha(relational_likert_questions)
###Find McDonald's omega for complete scale
omega_result<-psych::omega(relational_likert_questions)
print(omega_result)
##Find mean score, sd score, and item-total correlation (raw.r) values
alpha_result<-psych::alpha(relational_likert_questions)
alpha_result$item.stats

###Find mean sub-scale value & sd
WBData<- WBData %>% 
  mutate(RWB_Index = rowMeans(relational_likert_questions, na.rm = TRUE))
mean(WBData$RWB_Index)
sd(WBData$RWB_Index)


##Format the data from the complete scale so we can plot mean Item-Rest Correlation Values (+/- S.D.) for each dimensional sub-scale
Plotting_df<-alpha_result_complete$item.stats
Material_Prompts<-Plotting_df %>% dplyr::slice(c(1, 2, 6, 7))
Material_Prompts$Sub_Scale<-"Material"

Subjective_Prompts<-Plotting_df %>% dplyr::slice(c(6, 7, 8, 9))
Subjective_Prompts$Sub_Scale<-"Subjective"

Relational_Prompts<-Plotting_df %>% dplyr::slice(c(3, 4, 5))
Relational_Prompts$Sub_Scale<-"Relational"

All_Prompts<-rbind(Material_Prompts,Subjective_Prompts, Relational_Prompts)
All_Prompts$Sub_Scale<-as.factor(All_Prompts$Sub_Scale)

All_Prompts<-All_Prompts %>%
  mutate(Sub_Scale = fct_relevel(Sub_Scale, "Relational", "Subjective", "Material"))
All_Prompts$ID<-row.names(All_Prompts)

mean_sd_r_drop_by_subscale <- All_Prompts %>%
  group_by(Sub_Scale) %>%
  summarise(
    mean_r_drop = mean(r.drop, na.rm = TRUE),
    sd_r_drop = sd(r.drop, na.rm = TRUE)
  ) %>%
  ungroup()


####Create and export plot (i.e., Manuscript Figure 6B)

P<-ggplot(data = All_Prompts, aes(y = Sub_Scale, x = r.drop)) +
  geom_point(aes(x = r.drop, y = Sub_Scale, color=Sub_Scale), position = position_jitter(height = 0.3)) +
  geom_point(data=mean_sd_r_drop_by_subscale, aes(x=mean_r_drop, y=Sub_Scale, color=Sub_Scale, size=3), pch=18)+
  geom_errorbar(data = mean_sd_r_drop_by_subscale,
                 aes(xmin = mean_r_drop - sd_r_drop,
                     xmax = mean_r_drop + sd_r_drop,
                     x = mean_r_drop,
                     y = Sub_Scale, color=Sub_Scale), # Explicitly map 'y' again for safety
                 height = 0.1, # Controls the vertical thickness of the error bar caps
                 linewidth = .5) + theme_bw() +  scale_color_manual(values=c( "#D95F02", "#7570B3", "#1B9E77")) +ylab("Dimensional Sub-Scale")+ xlab("Item-Rest Correlation")

P+theme(legend.position = "none")

ggsave('WB_IR_Dotplot.tif',  plot = last_plot(), dpi=300, height=3.3, width=3, units='in')

####
####
####

####REPEAT THE PROCESS FOR BARRIERS DATA

Data<-Data%>% mutate_at(vars('Q13._1', 'Q13._2', 'Q13._3', 'Q13._4', 'Q13._5', 'Q13._6', 'Q13._7', 'Q13._8', 'Q13._9'), as.numeric)
BData<-Data[c(14:22)]

###Remove observations in instances where the question was not completely answered
BData<-BData[complete.cases(BData),]
likert_questions <- BData %>% dplyr::select(1:9)

###Find Cronbach's aplha for complete scale
cronbach.alpha(likert_questions)

###Find McDonald's omega for complete scale
omega_result<-psych::omega(likert_questions)
print(omega_result)

##Find mean score, sd score, and item-total correlation (raw.r) values
alpha_result_complete<-psych::alpha(likert_questions)
alpha_result_complete$item.stats


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

####REPEAT THE PROCESS FOR THE SUB-SCALLES

####SOCIAL BARRIERS
social_likert_questions <- BData %>% dplyr::select(1,2,9)

###Find Cronbach's aplha for complete scale
cronbach.alpha(social_likert_questions)
###Find McDonald's omega for complete scale
omega_result<-psych::omega(social_likert_questions)
print(omega_result)
##Find mean score, sd score, and item-total correlation (raw.r) values
alpha_result<-psych::alpha(social_likert_questions)
alpha_result$item.stats

###Find mean sub-scale value & sd
BData<- BData %>% 
  mutate(SB_Index = rowMeans(social_likert_questions, na.rm = TRUE))
mean(BData$SB_Index)
sd(BData$SB_Index)

###
###KNOWLEDGE BARRIERS
knowledge_likert_questions <- BData %>% dplyr::select(5,7,8)

###Find Cronbach's aplha for complete scale
cronbach.alpha(knowledge_likert_questions)
###Find McDonald's omega for complete scale
omega_result<-psych::omega(knowledge_likert_questions)
print(omega_result)
##Find mean score, sd score, and item-total correlation (raw.r) values
alpha_result<-psych::alpha(knowledge_likert_questions)
alpha_result$item.stats

###Find mean sub-scale value & sd
BData<- BData %>% 
  mutate(KB_Index = rowMeans(knowledge_likert_questions, na.rm = TRUE))
mean(BData$KB_Index)
sd(BData$KB_Index)

###
###PE BARRIERS
pe_likert_questions <- BData %>% dplyr::select(3,4,7)

###Find Cronbach's aplha for complete scale
cronbach.alpha(pe_likert_questions)
###Find McDonald's omega for complete scale
omega_result<-psych::omega(pe_likert_questions)
print(omega_result)
##Find mean score, sd score, and item-total correlation (raw.r) values
alpha_result<-psych::alpha(pe_likert_questions)
alpha_result$item.stats

###Find mean sub-scale value & sd
BData<- BData %>% 
  mutate(PEB_Index = rowMeans(pe_likert_questions, na.rm = TRUE))
mean(BData$PEB_Index)
sd(BData$PEB_Index)



##Format the data from the complete scale so we can plot mean Item-Rest Correlation Values (+/- S.D.) for each dimensional sub-scale


Plotting_df<-alpha_result_complete$item.stats
Personal_Prompts<-Plotting_df %>% dplyr::slice(c(1, 3, 6))
Personal_Prompts$Sub_Scale<-"Personal"

Social_Prompts<-Plotting_df %>% dplyr::slice(c(1, 2, 9))
Social_Prompts$Sub_Scale<-"Social"

PE_Prompts<-Plotting_df %>% dplyr::slice(c(3, 4, 7))
PE_Prompts$Sub_Scale<-"PE"

Knowledge_Prompts<-Plotting_df %>% dplyr::slice(c(5, 7,8))
Knowledge_Prompts$Sub_Scale<-"Knowledge"


All_Prompts<-rbind(Personal_Prompts,Knowledge_Prompts, Social_Prompts, PE_Prompts)
All_Prompts$Sub_Scale<-as.factor(All_Prompts$Sub_Scale)


mean_sd_r_drop_by_subscale <- All_Prompts %>%
  group_by(Sub_Scale) %>%
  summarise(
    mean_r_drop = mean(mean, na.rm = TRUE),
    sd_r_drop = sd(r.drop, na.rm = TRUE)
  ) %>%
  ungroup()

####Create and export plot (i.e., Manuscript Figure 4B)

P<-ggplot(data = All_Prompts, aes(y = Sub_Scale, x = r.drop))+
  geom_point(aes(x = r.drop, y = Sub_Scale, color=Sub_Scale), position = position_jitter(height = 0.3))+
 geom_point(data=mean_sd_r_drop_by_subscale, aes(x=mean_r_drop, y=Sub_Scale, color=Sub_Scale, size=3), pch=18) +
  geom_errorbarh(data = mean_sd_r_drop_by_subscale,
                 aes(xmin = mean_r_drop - sd_r_drop,
                     xmax = mean_r_drop + sd_r_drop,
                     x = mean_r_drop,
                     y = Sub_Scale, color=Sub_Scale), # Explicitly map 'y' again for safety
                 height = 0.1, # Controls the vertical thickness of the error bar caps
                 linewidth = .5) + theme_bw() +  scale_color_manual(values=c( "goldenrod2",  "steelblue3","mediumseagreen", "hotpink3")) +ylab("Dimensional Sub-Scale")+ xlab("Item-Rest Correlation")

P+theme(legend.position = "none")

ggsave('B_IR_Dotplot.tif',  plot = last_plot(), dpi=300, height=3.3, width=3, units='in')
