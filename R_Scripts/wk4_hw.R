library(rgdal)
library(sf)
library(tmap)
library(tmaptools)
library(RSQLite)
library(tidyverse)
library(here)
library(janitor)
library(countrycode)

HDI <- read.csv(here::here("CSVs", "Gender Inequality Index (GII).csv"), 
                skip = 5, header = TRUE, na.strings = "..", nrow = 189)
world <- st_read(here::here('Shapefiles', 'World_Countries_(Generalized)', 'World_Countries__Generalized_.shp'))%>%
  clean_names(.)



# Column Data
HDIcols <- HDI %>%
  clean_names() %>%
  select(country, x2019, x2010)%>%
  mutate(difference=x2019-x2010)%>%
  slice(1:189,)%>%
  mutate(iso_code=countrycode(country, origin = 'country.name', destination = 'iso2c'))

#select(!(starts_with("x_")))
#gend_inequ$"x" <- NULL
# t <- countrycode(HDIcols$country, origin = 'country.name', destination = 'iso2c')
# explantion of and CRAN https://github.com/vincentarelbundock/countrycode

plot(st_geometry(world))
summary(world)
summary(gend_inequ)
view(gend_inequ)

world_gend_ineq <- world %>%
  merge(., gend_inequ, 
        by.x="country" ,
        by.y="country")

world_gend_ineq <- world %>%
  merge(., gend_inequ, 
        by.x="country" ,
        by.y="country")


view(world_gend_ineq) 
# why does it come out with no rows :(
# how to gitignore my data



#Read in global gender inequality data
#Join the global gender inequality index to spatial data of the World, 
# creating a new column of difference in inequality between 2010 and 2019
