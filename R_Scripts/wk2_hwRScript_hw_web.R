library(dplyr)
library(tidyverse)
library(janitor)

#read from web
report_web <- read_csv("https://data.wa.gov/resource/5y3z-mgxd.csv",
                       col_names = TRUE,
                       locale = locale(encoding = "latin1"),
                       na = "NA") 
colnames(report_web)
summarize_all(report_web, class) %>%
  pivot_longer(everything(),
               names_to = "All_variables",
               values_to = "Variable_class")
view(report_web)

# wrangle
?read_csv
county_only <- report_web %>%
  clean_names() %>% 
  select(county, testsubject, percentmetstandard) %>%
  filter(testsubject == "Science") %>%
  #filter(str_detect(testsubject, "Science")) %>%
  #filter(county != "multiple")
  #subset(county)                                   #
  select(county, testsubject, percentmetstandard)         # select columns wanted
  filter(county != "Multiple")                              # Group by County
  # filter(test_subject == "Science") %>% 
  # #slice(101:120,)
  # filter(percent_met_standard != "Suppressed: N<10") %>%
  # filter(percent_met_standard != "No Students") %>%
  # filter(str_detect(percent_met_standard, "^>", negate = T)) %>%
  # mutate(percent_met_standard = str_replace_all(percent_met_standard, pattern = c('%' = ""))) %>%
  # mutate(percent_met_standard2 = as.numeric(percent_met_standard)) %>%
  # #negate = T is similar to does not equal
  # #groupby doesnt do anything, but summarize does. need both
  # group_by(county) %>%   #39 counties
  # summarise(average_met=mean(percent_met_standard2, na.rm=T)) %>%
  # # filter(percent_met_standard !=
  # view(county_only)


shape <-
  

?filter
#no rows coming up 
#when to use dplyr::select
#when to use setwd("/Users/yun/Documents/CASA/GIS/wk2/hw")    #???
#when to use here::here()
#filter(str_detect(`testsubject`, 'Science')) %>%



# Group by County
County_SciAve <- Science %>%
  group_by(county) %>%
  summarise(county_met = sum(countmetstandard), 
            county_expected = sum(count_of_students_expected), 
            county_percent_met = mean(county_met/county_expected))
view(County_SciAve)

# Calculate Average % met standards by County; and WA baseline
# mutate(ave_percent_met = 

# Description Above/Below WA Average
School_Report_Card2 <- School_Report_Card %>%
  mutate(Washington_compare = case_when(ave_percent_met>81.16 ~ "above WA average",
                               TRUE ~ "below WA average"))
School_Report_Card2




# County, Science, meet standards, average %
# Map county average % above/below Washington average
                                   