library(tidyverse)
library(here)
library(sf)
library(janitor)

report <- read_csv(here::here("CSVs", 
                              "Report_Card_Assessment_Data_2018-19_School_Year.csv"))

shape <- st_read(here::here("Shapefiles",
                            "Washington_Counties_with_Natural_Shoreline___washsh_area",
                            "Washington_Counties_with_Natural_Shoreline___washsh_area.shp"))

county_only <- report %>%
  clean_names() %>%
  # select just the useful columns
  select(county, test_subject, count_met_standard, 
         count_of_students_expected_to_test, grade_level)%>%
  # != means don't select this, but select everything else; rid of these rows
  filter(county != "Multiple")%>%
  # only want science rows
  filter(test_subject == "Science")%>%
  # only want rows for all grades
  filter(grade_level=="All Grades")%>%
  # group the schools by county
  group_by(county)%>%
  # sum the number meeting standards per county
  summarise(total_county_met_standard = sum(count_met_standard, na.rm=T), 
            #sum the total testtakers per county
            total_county_to_test = sum(count_of_students_expected_to_test, na.rm=T))%>%
  # make the ratio of these two summaries into a new column
  mutate(percent_met_per_county=(total_county_met_standard/total_county_to_test)*100)

## the average percent met for ALL the counties 
state_met<-sum(county_only$total_county_met_standard)
state_test<-sum(county_only$total_county_to_test)
state_that_met<-(state_met/state_test*100)

###percent over or under
county_to_state_difference <- county_only %>%
  # rename percent met to STATE DIFF
  mutate(state_diff = percent_met_per_county-state_that_met)%>%
  # what's the point of across() only one column
  mutate(across(state_diff, round, 1))
  
## now we need to join, usually it's best to use a unique ID, but we can do it with strings.

joined_data <- shape %>% 
  clean_names() %>%
  left_join(., 
            county_to_state_difference,
            by = c("countylabe" = "county"))

# If the strings didn't match (e.g. lower and upper case) we can covert them with...

t <- shape %>% 
  mutate(COUNTY2 = tolower(COUNTY))

### let's map

library(tmap)
library(tmaptools)

bbox_county <- joined_data %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "esri", zoom = NULL)

tm_shape(bbox_county)+
  tm_rgb()+
  
  tm_shape(joined_data) + 
  tm_polygons("state_diff", 
              style="pretty",
              palette="Blues",
              midpoint=NA,
              #title="Number of years",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "County to state percent difference in meeting science standards", 
            legend.position = c("right", "bottom"))



  
