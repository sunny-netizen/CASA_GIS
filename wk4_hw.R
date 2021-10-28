library(rgdal)
library(sf)
library(tmap)
library(tmaptools)
library(RSQLite)
library(tidyverse)
library(here)
library(janitor)

world <- st_read("Data/World_Countries_(Generalized)/World_Countries__Generalized_.shp") %>%
  clean_names(.)
view(world)
gend_ineq <- read.csv("Data/Gender Inequality Index (GII).csv", 
                        skip = 5, header = TRUE, na.strings = "..", nrow = 189)
gend_inequ <- gend_ineq %>%
  clean_names() %>%
  select(!(starts_with("x_")))
gend_inequ$"x" <- NULL

plot(st_geometry(world))
summary(world)
summary(gend_inequ)
view(gend_inequ)

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
