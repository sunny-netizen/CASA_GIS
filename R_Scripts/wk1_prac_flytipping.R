library(rgdal) 
library(sf) 
library(tidyverse)
library(tmap) 
library(tmaptools)
library(RSQLite)
library(readr)
library(here)

#read in the shapefile
shape <- st_read(here::here('Shapefiles', 'statistical-gis-boundaries-london', 
                            'ESRI', 'London_Borough_Excluding_MHW.shp'))
summary(shape)
#plotting
plot(shape)
#plot the outline of shapefile
shape %>%
  st_geometry() %>%
  plot()

# read in the csv
mycsv <- read_csv(here::here('CSVs', 'fly_tipping_borough_edit.csv'), skip=1)
mycsv 

# merge csv and shapefile
shape <- shape%>%
  merge(.,
        mycsv,
        by.x="GSS_CODE", 
        by.y="Row Labels")
shape%>%
  head(., n=10)

# set tmap to plot
tmap_mode("plot")
shape %>%
  qtm(.,fill = "2011-12") # same as qtm(shape, fill = "2011_12")

# write to a .gpkg
shape %>%
  #st_write(sf object, 'filepath.gpkg', 'name for sf object')
  st_write(., here::here('Geopackages', 'wk1_prac.gpkg'), "london_boroughs_fly_tipping", 
           delete_layer=TRUE) #overwrites existing file if code is rerun

# connect to the .gpkg
con <- dbConnect(SQLite(),dbname=here::here('Geopackages', 'wk1_prac.gpkg') )

# list what is in it
con %>%
  dbListTables()

# add the original csv to gpkg as well
con %>%
  #dbWriteTable(conn, name=dbms table name, value=dataframe, ...)
  #Writes, overwrites or appends a data frame to a database table
  dbWriteTable(., 
               "original_csv",  #database table name to add
               mycsv,   #dataframe to append to the database table
               overwrite=TRUE) #overwrites existing file if code is rerun

# list what is in it
con %>%
  dbListTables()

#disconnect
con %>% 
  dbDisconnect()
