library(rgdal)
library(sf)
library(tmap) 
library(tmaptools)
library(RSQLite)
library(tidyverse)

#read in the shapefile
shape <- st_read(here::here('Shapefiles', 
                            'statsnzterritorial-authority-2018-generalised-SHP', 
                            'territorial-authority-2018-generalised.shp'))

# read in the csv
mycsv <- read_csv(here::here('CSVs', 'paid_employee_2018.csv'))
summary(mycsv)

# merge csv and shapefile
shape <- shape%>%
  merge(.,
        mycsv,
        by.x="TA2018_V1_", 
        by.y="Row Labels")
summary(shape)

# set tmap to plot
tmap_mode("plot")
shape %>%
  qtm(.,fill = "Sum of Census_2018_Industry_by_workplace_address_Total_CURP_employed_15years_and_over")

# write to a .gpkg
shape %>%
  st_write(., here::here('Geopackages', 'wk1_hw.gpkg'), "nz_terr_auth_paid_empl", 
           delete_layer=TRUE)

# connect to the .gpkg
con <- dbConnect(SQLite(),dbname=here::here('Geopackages', 'wk1_hw.gpkg'))

# list what is in it
con %>%
  dbListTables()

# add the original csv to gpkg as well
con %>%
  dbWriteTable(., 
               "paid_employee_2018_csv",  #database table name to be overwritten
               mycsv,   #dataframe to overwrite or append to the database table
               overwrite=TRUE)

# list what is in it
con %>%
  dbListTables()

# disconnect from it
con %>% 
  dbDisconnect()