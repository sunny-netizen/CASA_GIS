library(rgdal)
library(dplyr)
library(tidyverse)
library(janitor)
library(plotly)
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(geojsonio)
library(here)
library(OpenStreetMap)
library(osmdata)

# csv
report_local <- read.csv(here::here("CSVs", "Report_Card_Assessment_Data_2018-19_School_Year.csv"),
                         header = TRUE, 
                         sep = ",",  
                         encoding = "latin1")

# wrangle
county_ave <- report_local %>%
  clean_names() %>%
  #colnames(county_ave)
  #unique(county_ave["test_subject"])
  select(county, test_subject, 
         count_of_students_expected_to_test_including_previously_passed,
         count_met_standard) %>%
  filter(test_subject == "Science") %>%
  filter(county != "Multiple") %>%
  filter(county != "") %>%
  filter(count_met_standard != "NA") %>%
  group_by(county) %>%
  summarise(county_met = sum(count_met_standard), 
            county_testers = sum(count_of_students_expected_to_test_including_previously_passed),
            county_percent_met = round(mean(county_met/county_testers), digits = 3))

county_met = sum(county_ave$county_met)
county_sum = sum(county_ave$county_testers)
WA_ave = county_met/county_sum                  
                  
CompareWA <- county_ave %>%
  mutate(WA_compare = case_when(county_percent_met > WA_ave ~"above WA average", TRUE ~ "below WA average"))

# csv
shape <- st_read(here::here("Shapefiles", "Washington_Counties_with_Natural_Shoreline___washsh_area", "Washington_Counties_with_Natural_Shoreline___washsh_area.shp"))

# join
good_science <- shape %>%
  left_join(.,
            CompareWA, by = c("COUNTYLABE" = "county"))

# map
tmap_mode("plot")
qtm(good_science, 
    fill = "county_percent_met")
qtm(good_science, 
    fill = "WA_compare")

# OSM

library(rJava)

WA_osm <- good_science %>%
  st_bbox(.) %>%
  tmaptools::read_osm(., type = "osm", zoom = NULL)


bbox_county <- shape %>%
  st_bbox(.) %>%
  tmaptools::read_osm(., type = "esri", zoom = NULL)



# County, Science, meet standards, average %
# Map county average % above/below Washington average



