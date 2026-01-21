###Reproduces Manuscript Figure 3

rm(list = ls(all = TRUE))

library(stringr)      ###Version 1.5.1
library(ggplot2)      ###Version 3.5.1
library(ggcorrplot)   ###Version 0.1.4.1

###Load in data from your local machine
Data<-read.csv("C:/Users/tfrawley/Desktop/OPC_Analysis/Survey_Data_For_Analysis_12.19.2025.csv")


###Reformat Area Variable as Dummy Variable

Data$North<-str_detect(Data$Area, regex("North", ignore_case=FALSE)) 
Data$North<-ifelse(Data$North==TRUE, 1, 0) 
Data$North-as.numeric(Data$North)

Data$South<-str_detect(Data$Area, regex("South", ignore_case=FALSE)) 
Data$South<-ifelse(Data$South==TRUE, 1, 0) 
Data$South<-as.numeric(Data$South)

Data$Central<-str_detect(Data$Area, regex("Central", ignore_case=FALSE)) 
Data$Central<-ifelse(Data$Central==TRUE, 1, 0) 
Data$Central<-as.numeric(Data$Central)


###Subset to only retain predictor variables of interest
Subset <- Data[c(3, 25:34, 36:40)]
Subset <- Subset[, c( "Usage",  "Income",
                      "EJ_Bin", "Male", "Female", "NB", "White", "Asian","Pacific_Islander", "Latino", "Black", "Native", "Distance", "South", "Central", "North")]

###Compute correlation matrix
corr <- round(cor(Subset, use="pairwise.complete.obs", method="spearman"), 4)


###Obtain associated p Values
p_values <- matrix(NA, nrow = ncol(corr), ncol = ncol(corr))

for (i in 1:ncol(corr)) {
  for (j in 1:ncol(corr)) {
    if (i != j) {
      p_values[i, j] <- cor.test(Subset[, i], Subset[, j], method="spearman", use="pairwise.complete.obs")$p.value
    }
  }
}

# Apply the Bonferroni correction (adjust p-values)
p_values_adjusted <- p.adjust(p_values[lower.tri(p_values)], method = "bonferroni")


# Assign corrected p-values back into the matrix
p_values[lower.tri(p_values)] <- p_values_adjusted
p_values[upper.tri(p_values)] <- t(p_values)[upper.tri(p_values)]


##Plotting Adjustments to reduce scale extent
corr[corr < (-0.4)] <-0
corr[corr > (0.4)] <-0
corr[15, 14] <- 0


corr<-as.data.frame(corr)


ggcorrplot(corr, type="lower",
           p.mat=p_values,
           insig="blank",
           lab=TRUE,
           lab_size=2,
           digits=2,
           method="circle",
           ggtheme = theme_minimal()) +
  scale_fill_gradient2(
    low = "red3", mid = "white", high = "navyblue", midpoint = 0,
    limits = c(-0.5, 0.5))

ggsave('Predictor_Correlations.png',  plot = last_plot(), dpi=300, height=5, width=7.5, units='in')






