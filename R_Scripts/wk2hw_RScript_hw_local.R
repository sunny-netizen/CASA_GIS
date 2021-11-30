library(dplyr)
library(tidyverse)
library(janitor)
#when to use setwd
#when to use here::here()


#read local csv file
report_local <- read.csv("Report_Card_Assessment_Data_2018-19_School_Year.csv", 
                         header = TRUE, 
                         sep = ",",  
                         encoding = "latin1")
colnames(report_local)
unique(report_local[c("County")])
summarize_all(report_local, class) %>%
  pivot_longer(everything(),
               names_to = "All_variables",
               values_to = "Variable_class")
view(report_local)

# wrangle bad science
unit_ave <- report_local %>%
  #clean_names() %>%
  select(County, TestSubject, PercentMetStandard) %>%
  filter(TestSubject == "Science") %>%
  filter(County != "Multiple") %>%
  filter(County != "") %>%
  #subset(County.....)
  filter(PercentMetStandard != "Suppressed: N<10") %>%
  filter(PercentMetStandard != "No Students") %>%
  filter(str_detect(PercentMetStandard, "^<", negate = T)) %>%
  filter(str_detect(PercentMetStandard, "^>", negate = T)) %>%
  mutate(PercentMetStandard = str_replace_all(PercentMetStandard, pattern = c('%' = ""))) %>%
  mutate(PercentMetStandard = as.numeric(PercentMetStandard)) %>%
  group_by(County) %>%   #39 counties
  summarise(percent_ave_met = round(mean(PercentMetStandard, na.rm=T), digits = 2))


view(unit_ave)
summarize_all(unit_ave, class) %>%
  pivot_longer(everything(),
               names_to = "All_variables",
               values_to = "Variable_class")

#wrangle good science
county_ave <- report_local %>%
  select(County, TestSubject, 
         Count.of.students.expected.to.test.including.previously.passed,
         CountMetStandard) %>%
  filter(TestSubject == "Science") %>%
  filter(County != "Multiple") %>%
  filter(County != "") %>%
  filter(CountMetStandard != "NA") %>%
  group_by(County) %>%
  summarise(county_met = sum(CountMetStandard), 
            county_testers = sum(Count.of.students.expected.to.test.including.previously.passed),
            county_percent_met = round(mean(county_met/county_testers), digits = 3)) %>%
  mutate(WA_compare = case_when(county_percent_met > 
                                  sum((county_ave$county_met)/sum(county_ave$county_testers)) 
                     ~"above WA average", TRUE ~ "below WA average"))

view(county_ave)
summarize_all(county_ave, class) %>%
  pivot_longer(everything(),
               names_to = "All_variables",
               values_to = "Variable_class")


#plotting
#plot(county_ave$County, county_ave$county_percent_met)

library(plotly)
plot_ly(unit_ave, 
        #data for x axis
        x = ~County, 
        #data for y axis
        y = ~percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14, 
        #attribute to display when hovering 
        text = ~borough, 
        type = "scatter", 
        mode = "markers")
plot_ly(county_ave, 
        x = ~County, 
        y = ~county_percent_met, 
        text = ~County, 
        type = "bar",
        mode = "markers")

#mapping
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(tmap)
library(tmaptools)


shape <- st_read("Washington_Counties_with_Natural_Shoreline___washsh_area/Washington_Counties_with_Natural_Shoreline___washsh_area.shp")
# https://opendata.arcgis.com/datasets/c91c002dccea4ec9a5ac8f0d889ef6bb_122.geojson

good_science <- shape %>%
  left_join(.,
            county_ave, by = c("COUNTYLABE" = "County"))
  #how to do merge way?

tmap_mode("plot")
qtm(good_science, 
    fill = "county_percent_met")
qtm(good_science, 
    fill = "WA_compare")

install.packages("osmdata")
library(osmdata)

WA_osm <- good_science %>%
  st_bbox(.) %>%
  tmaptools::read_osm(., type = "osm", zoom = NULL)



# County, Science, meet standards, average %
# Map county average % above/below Washington average
                                   
                                   
                                   