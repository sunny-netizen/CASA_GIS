#setwd("/Users/yun/Documents/CASA/GIS")
A <- 1
B <- 3
C <- A+B
C
ls()

Hide#create some datasets, first a vector of 1-100 and 101-200
Data1 <- c(1:100)
Data2 <- c(101:200)
#Plot the data
plot(Data1, Data2, col="red")

Hide#just for fun, create some more, this time some normally distributed
#vectors of 100 numbers
Data3 <- rnorm(100, mean = 53, sd=34)
Data4 <- rnorm(100, mean = 64, sd=14)
#plot
plot(Data3, Data4, col="blue")

df <- data.frame(Data1, Data2)
plot(df, col="green")

library(tidyverse)
#show the first 10 and then last 10 rows of data in df...
df %>%
  head()

df %>%
  tail()

df[1:10, 1]
df[5:15,]
df[c(2,3,6),2]
df[,1]


library(dplyr)
df <- df %>%
  dplyr::rename(column1 = Data1, column2=Data2)

df %>% 
  dplyr::select(column1)

df$column1
df["column"]
df[["column1"]]


# START HERE
library(rgdal)
library(sf)

LondonDataOSK<- read.csv(here::here("CSVs", "old_skool_cleaning.csv"), 
                         header = TRUE, 
                         sep = ",",  
                         encoding = "latin1")
library(here)
here::here()
LondonDataOSK<- read.csv(here::here("wk2/practical/practical_data", "old_skool_cleaning.csv"))


#wang the data in straight from the web using read_csv, 
#skipping over the 'n/a' entries as you go...
LondonData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a") #exclude "n/a" values
LondonData
class(LondonData)
class(LondonDataOSK)

Datatypelist <- LondonData %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")
Datatypelist


LondonData%>%
  colnames()%>%
  # just look at the head, top5
  head()

#Selecting rows
LondonBoroughs<-LondonData[626:658,]
LondonBoroughs<-LondonData%>%
  slice(626:658)
Femalelifeexp<- LondonData %>% 
  filter(`Female life expectancy -2009-13`>90)
LondonBoroughs<- LondonData %>% 
  filter(str_detect(`New code`, "^E09"))
LondonBoroughs$`Ward name`
LondonBoroughs %>% 
  dplyr::select(`Ward name`) %>%
  print()
LondonBoroughs<-LondonBoroughs %>%
  distinct()
LondonBoroughs

#select columns
LondonBoroughs_manualcols<-LondonBoroughs[,c(1,19,20,21)]
LondonBoroughs_dplyrcols<-LondonBoroughs %>%
  dplyr::select(c(1,19,20,21))
LondonBoroughs_contains<-LondonBoroughs %>% 
  dplyr::select(contains("expectancy"), 
                contains("obese - 2011/12 to 2013/14"),
                contains("Ward name"))   

#rename columns
install.packages("janitor")
library(janitor)

LondonBoroughs <- LondonBoroughs %>%
  dplyr::rename(Borough=`Ward name`)%>%
  clean_names()

#the average of male and female life expectancy together
Life_expectancy <- LondonBoroughs %>% 
  #new column with average of male and female life expectancy
  mutate(averagelifeexpectancy= (female_life_expectancy_2009_13 +
                                   male_life_expectancy_2009_13)/2)%>%
  #new column with normalised life expectancy
  mutate(normalisedlifeepectancy= averagelifeexpectancy /
           mean(averagelifeexpectancy))%>%
  #select only columns we want
  dplyr::select(new_code,
                borough,
                averagelifeexpectancy, 
                normalisedlifeepectancy)%>%
  #arrange in descending order
  #ascending is the default and would be
  #arrange(normalisedlifeepectancy)
  arrange(desc(normalisedlifeepectancy))

slice_head(Life_expectancy, n=5)
slice_tail(Life_expectancy,n=5)


Life_expectancy2 <- Life_expectancy %>%
  mutate(UKcompare = case_when(averagelifeexpectancy>81.16 ~ "above UK average",
                               TRUE ~ "below UK average"))
Life_expectancy2


Life_expectancy2_group <- Life_expectancy2 %>%
  mutate(UKdiff = averagelifeexpectancy-81.16) %>%
  group_by(UKcompare)%>%
  summarise(range=max(UKdiff)-min(UKdiff), count=n(), Average=mean(UKdiff))

Life_expectancy2_group

Life_expectancy3 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(where(is.numeric), round, 3))%>%
  mutate(across(UKdiff, round, 0))%>%
  mutate(UKcompare = case_when(averagelifeexpectancy >= 81 ~ 
                                 str_c("equal or above UK average by",
                                       UKdiff, 
                                       "years", 
                                       sep=" "), 
                               TRUE ~ str_c("below UK average by",
                                            UKdiff,
                                            "years",
                                            sep=" ")))%>%
  group_by(UKcompare)%>%
  summarise(count=n())

Life_expectancy3


Life_expectancy4 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(is.numeric, round, 3))%>%
  mutate(across(UKdiff, round, 0))

plot(LondonBoroughs$male_life_expectancy_2009_13,
     LondonBoroughs$percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14)

library(plotly)
plot_ly(LondonBoroughs, 
        #data for x axis
        x = ~male_life_expectancy_2009_13, 
        #data for y axis
        y = ~percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14, 
        #attribute to display when hovering 
        text = ~borough, 
        type = "scatter", 
        mode = "markers")

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

# this will take a few minutes
#EW <- st_read("https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson")

# geojson in local folder
EW <- st_read(here::here("GeoJSONs", "Local_Authority_Districts_(December_2015)_Boundaries.geojson"))

LondonMap<- EW %>%
  filter(str_detect(lad15cd, "^E09"))

#plot it using the qtm function
qtm(LondonMap)

#Join some more data to Boundaries (EW)
LondonData <- clean_names(LondonData)

#EW is the data we read in straight from the web
BoroughDataMap <- EW %>%
  clean_names()%>%
  # the . here just means use the data already loaded
  filter(str_detect(lad15cd, "^E09"))%>%
  merge(.,
        LondonData, 
        by.x="lad15cd", 
        by.y="new_code",
        no.dups = TRUE)%>%
  distinct(.,lad15cd, 
           .keep_all = TRUE)

BoroughDataMap2 <- EW %>% 
  clean_names() %>%
  filter(str_detect(lad15cd, "^E09"))%>%
  left_join(., 
            LondonData,
            by = c("lad15cd" = "new_code"))

tmap_mode("plot")
qtm(BoroughDataMap, 
    fill = "rate_of_job_seekers_allowance_jsa_claimants_2015")

tmaplondon <- BoroughDataMap %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "osm", zoom = NULL)

