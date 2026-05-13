###Reproduces Manuscript Figure 2

rm(list = ls(all = TRUE))

library(sf)
library(raster)
library(ggplot2)
library(viridis)
library(dplyr)
library(ggspatial)
library(tigris)

options(tigris_use_cache = TRUE)


#### Load Survey Data


Data <- read.csv("C:/Users/tfrawley/Desktop/OPC_Project/OPC_Analysis/Survey_Data_For_Analysis_12.19.2025.csv")

Data <- Data[c(1, 2, 37)]
names(Data)[1] <- "ZIP_CODE"

Zip_Totals <- aggregate(ResponseId ~ ZIP_CODE, FUN = length, data = Data)
Zip_Totals$ZIP_CODE <- as.numeric(Zip_Totals$ZIP_CODE)


#### Pull California boundaries from US Census Bureau (public domain TIGER/Line)


ca_state <- states(cb = TRUE, year = 2020) %>%
  filter(STUSPS == "CA")
ca_counties <- counties(state = "CA", cb = TRUE, year = 2020)
us_zctas <- zctas(year = 2020, cb = TRUE)

#### Transform everything to WGS84
ca_state    <- st_transform(ca_state,    crs = 4326)
ca_counties <- st_transform(ca_counties, crs = 4326)
us_zctas    <- st_transform(us_zctas,    crs = 4326)

#### Filter ZCTAs to California
us_zctas$zip_num <- as.numeric(us_zctas$ZCTA5CE20)
ca_candidates <- us_zctas[us_zctas$zip_num >= 90001 &
                          us_zctas$zip_num <= 96162, ]
ca_zctas <- ca_candidates[lengths(st_intersects(ca_candidates, ca_state)) > 0, ]
ca_zctas$ZIP_CODE <- ca_zctas$zip_num

CA_Zips <- ca_zctas[, c("ZIP_CODE", "geometry")]



##### Merge polygons with respondent counts

CA_Zips <- merge(CA_Zips, Zip_Totals, by = "ZIP_CODE", all.x = TRUE)
names(CA_Zips)[names(CA_Zips) == "ResponseId"] <- "Respondent_Number"

##### Plot — All California


ggplot() +
  geom_sf(data = ca_state,    fill = "grey50", color = NA) +
  geom_sf(data = CA_Zips,     aes(fill = Respondent_Number), color = "grey35", lwd = 0.25) +
  scale_fill_viridis(option = "D") + theme_bw() +
  geom_rect(aes(xmin = -122.3, xmax = -121.3, ymin = 36.5, ymax = 37.2),
            color = "black", lwd = 1, fill = NA) +
  geom_rect(aes(xmin = -122.6, xmax = -122.1, ymin = 37.5, ymax = 38),
            color = "black", lwd = 1, fill = NA) +
  geom_rect(aes(xmin = -121, xmax = -118.5, ymin = 33.75, ymax = 35),
            color = "black", lwd = 1, fill = NA) +
  annotation_scale(
    location = "tr",
    width_hint = 0.4,
    style = "bar",
    text_cex = 1.25
  )

##ggsave('All_CA_Map.tif', plot = last_plot(), dpi = 300, height = 7, width = 7, units = 'in')

##### Community Vulnerability Overlay


EJ_Metrics <- Data[c(1, 3)]
EJ_Metrics <- distinct(EJ_Metrics)
CA_Zips <- merge(CA_Zips, EJ_Metrics, by = "ZIP_CODE", all.x = TRUE)

Disadvantaged   <- CA_Zips[which(CA_Zips$EJ_Bin == 4), ]
S_Disadvantaged <- CA_Zips[which(CA_Zips$EJ_Bin == 5), ]

#### Plot — Monterey Bay Area inset


ggplot() +
  geom_sf(data = ca_state,    fill = "grey50", color = NA) +
  geom_sf(data = CA_Zips,     aes(fill = Respondent_Number), color = "grey35", lwd = 0.25) +
  geom_sf(data = Disadvantaged,   fill = NA, color = "orange2", lwd = 0.5) +
  geom_sf(data = S_Disadvantaged, fill = NA, color = "red",    lwd = 0.75) +
  scale_fill_viridis(option = "D") +
  coord_sf(xlim = c(-122.3, -121.3), ylim = c(36.5, 37.2), expand = FALSE) +
  theme_bw()

##ggsave('Monterey_Map.tif', plot = last_plot(), dpi = 300, height = 2.75, width = 7, units = 'in')



##### Plot — Santa Barbara / Channel Islands inset


ggplot() +
  geom_sf(data = ca_state,    fill = "grey50", color = NA) +
  geom_sf(data = CA_Zips,     aes(fill = Respondent_Number), color = "grey35", lwd = 0.25) +
  geom_sf(data = Disadvantaged,   fill = NA, color = "orange2", lwd = 0.5) +
  geom_sf(data = S_Disadvantaged, fill = NA, color = "red",     lwd = 0.75) +
  scale_fill_viridis(option = "D") +
  coord_sf(xlim = c(-121, -118.5), ylim = c(33.75, 35), expand = FALSE) +
  theme_bw()

###ggsave('SB_Map.tif', plot = last_plot(), dpi = 300, height = 2.75, width = 7, units = 'in')



ggplot() +
  geom_sf(data = ca_state,    fill = "grey50", color = NA) +
  geom_sf(data = CA_Zips,     aes(fill = Respondent_Number), color = "grey35", lwd = 0.25) +
  geom_sf(data = Disadvantaged,   fill = NA, color = "orange2", lwd = 0.5) +
  geom_sf(data = S_Disadvantaged, fill = NA, color = "red",     lwd = 0.75) +
  scale_fill_viridis(option = "D") +
  coord_sf(xlim = c(-122.6, -122.1), ylim = c(37.5, 38), expand = FALSE) +
  theme_bw()

###ggsave('SF_Map.tif', plot = last_plot(), dpi = 300, height = 4.75, width = 7, units = 'in')





