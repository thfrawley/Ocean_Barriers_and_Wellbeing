rm(list = ls(all = TRUE))

library(sf)
library(raster)
library(ggplot2)
library(viridis)
library(dplyr)
library(ggspatial)



###Load in Survey Data from where it is stored on your local maching
Data<-read.csv("C:/Users/tfrawley/Desktop/OPC_Project/OPC_Analysis/Survey_Data_For_Analysis_12.19.2025.csv")


##Only Retain Columns where Zip Code info is reported and an adjacent column to facilitate aggregating
Data<-Data[c(1,2,37)]
names(Data)[1]<-"ZIP_CODE"

##Find the total number for each zip code
Zip_Totals<-aggregate(ResponseId~ZIP_CODE, FUN=length, data=Data)
Zip_Totals$ZIP_CODE<-as.numeric(Zip_Totals$ZIP_CODE)

###Read in ZipCode Shapefile and set Projection to WGS84
###This file can be downloaded from many sources online including https://gis.data.ca.gov/datasets/ca-zip-code-boundaries/about
CA_Zips<-read_sf('C:/Users/tfrawley/Desktop/OPC_Project/OPC_Analysis/California_Zip_Codes.shp')
CA_Zips<-st_transform(CA_Zips, crs=4326)

##Merge Simple Features Spatial File with Zip Code Totals File
CA_Zips<-merge(CA_Zips, Zip_Totals, by="ZIP_CODE", all.x=TRUE)
names(CA_Zips)[7]<-"Respondent_Number"

##Plot & Save All CA MAP

ggplot()+
  geom_sf(data=CA_Zips, aes(fill=Respondent_Number), lwd=.5)+
  scale_fill_viridis(option = "D") + theme_bw()  +
  geom_rect(aes(xmin = -122.3, xmax = -121.3, ymin = 36.5, ymax = 37.2), color = "black", lwd=1, fill = NA) +
  geom_rect(aes(xmin = -122.6, xmax = -122.1, ymin = 37.5, ymax = 38), color = "black", lwd=1, fill = NA) +
  geom_rect(aes(xmin = -121, xmax = -118.5, ymin = 33.75, ymax = 35), color = "black", lwd=1, fill = NA) +
  annotation_scale(
    location = "tr",    # "bl" = bottom left (options: "tl", "tr", "br", "bl")
    width_hint = 0.3,   # Adjusts the width relative to the plot size
    style = "bar",
    text_cex=1.25
  )


###ggsave('All_CA_Map.tif',  plot = last_plot(), dpi=300, height=7, width=7, units='in')


###Outline Zip codes according to relative Community Vulnerability as inferred by CalEnviroScreen Scores

EJ_Metrics<-Data[c(1,3)]
EJ_Metrics <- distinct(EJ_Metrics)
CA_Zips<-merge(CA_Zips, EJ_Metrics, by="ZIP_CODE", all.x=TRUE)

###Create a Layer for Communities of High Vulnerability
Disadvantaged<-CA_Zips[which(CA_Zips$EJ_Bin==4),]
###Creat a Layer for Communities with Very High Vulnerabilty
S_Disadvantaged<-CA_Zips[which(CA_Zips$EJ_Bin==5),]


##Plot & Save Monterey Bay Area

ggplot()+
  geom_sf(data=CA_Zips, aes(fill=Respondent_Number), lwd=.5) +
  geom_sf(data=Disadvantaged, fill=NA, color="orange", lwd=.5) +
  geom_sf(data=S_Disadvantaged, fill=NA, color="red", lwd=.75) +
  scale_fill_viridis(option = "D")+ylim(36.5,37.2)  + xlim(-122.3,-121.3) +theme_bw()

##ggsave('Monterey_Map.tif',  plot = last_plot(), dpi=300, height=2.75, width=7, units='in')

##Plot & Save Santa Barbara

ggplot()+
  geom_sf(data=CA_Zips, aes(fill=Respondent_Number), lwd=.5) +
  geom_sf(data=Disadvantaged, fill=NA, color="orange2", lwd=.5) +
  geom_sf(data=S_Disadvantaged, fill=NA, color="red", lwd=.75) +
  scale_fill_viridis(option = "D")+ylim(33.75,35)  + xlim(-121,-118.5) + theme_bw()

##ggsave('SB_Map.tif',  plot = last_plot(), dpi=300, height=2.75, width=7, units='in')

###Plot & Save San Francisco

ggplot()+
  geom_sf(data=CA_Zips, aes(fill=Respondent_Number), lwd=.5)+
  geom_sf(data=Disadvantaged, fill=NA, color="orange2", lwd=.5) +
  geom_sf(data=S_Disadvantaged, fill=NA, color="red", lwd=.75) +
  scale_fill_viridis(option = "D")+ylim(37.5,38)  + xlim(-122.6,-122.1) +theme_bw()

###ggsave('SF_Map.tif',  plot = last_plot(), dpi=300, height=4.75, width=7, units='in')


getwd()
