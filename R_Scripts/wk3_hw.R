library(sf)
library(here)
library(janitor)
library(rgdal)
library(sp)
library(raster)
library(ggplot2)
library(tidyverse)

# VECTOR DATA from GADM
brazil <- st_read(here("hw3_data", "gadm36_BRA.gpkg"),
                        layer='gadm36_BRA_0')     # why this???

#Cities Shapefile from ESRI
world_cities <- st_read("/Users/yun/Documents/CASA/GIS/wk3/hw3_data/World_Cities/World_Cities.shp")%>%
  clean_names()
brazil_cities <- World_cities %>%
  filter(cntry_name == "Brazil")

# RASTER DATA from WorldClim BCC-CSM2-MR. Read in with raster(), stack(), or brick()
ssp1_brazil <- brick(here("hw3_data", "wc2.1_2.5m_tmax_BCC-CSM2-MR_ssp126_2081-2100.tif"))%>%
  crop(., brazil) %>%
  mask(., brazil)
ssp5_brazil <- brick(here("hw3_data", "wc2.1_2.5m_tmax_BCC-CSM2-MR_ssp585_2081-2100.tif"))%>%
  crop(., brazil) %>%
  mask(., brazil) ## need na.rm???
sspdiff_brazil = ssp5_brazil - ssp1_brazil

month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(sspdiff_brazil) <- month
sspdiff_brazil$Jan

# Extract raster data for cities
brazil_city_diff <- raster::extract(sspdiff_brazil, brazil_cities, method = "bilinear")
?raster::extract
brazil_city_diff

### untidy
almost_tidy <- brazil_city_diff %>%
  #drop_na()%>% might misalign data?
  as_tibble()%>% ###when is this needed?
  add_column(Site = brazil_cities$city_name, .before = "Jan")
view(almost_tidy)

tidy_city <- almost_tidy %>%
  pivot_longer(everything(), 
               names_to="Months", 
               values_to="temp_diff")


### each month a column (variable)
brazil_city_diff2 <- brazil_cities %>% 
  add_column(brazil_city_diffJan=brazil_city_diff[,1])%>%
  add_column(brazil_city_diffFeb=brazil_city_diff[,2])%>%
  add_column(brazil_city_diffMar=brazil_city_diff[,3])%>%
  add_column(brazil_city_diffApril=brazil_city_diff[,4])%>%
  add_column(brazil_city_diffMay=brazil_city_diff[,5])%>%
  add_column(brazil_city_diffJune=brazil_city_diff[,6])%>%
  add_column(brazil_city_diffJuly=brazil_city_diff[,7])%>%
  add_column(brazil_city_diffAug=brazil_city_diff[,8])%>%
  add_column(brazil_city_diffSept=brazil_city_diff[,9])%>%
  add_column(brazil_city_diffOct=brazil_city_diff[,10])%>%
  add_column(brazil_city_diffNov=brazil_city_diff[,11])%>%
  add_column(brazil_city_diffDec=brazil_city_diff[,12])
view(brazil_city_diff2)

city_climate_diff <- brazil_city_diff2 %>%
  dplyr::select(contains("brazil_city_diff"))%>%
  st_drop_geometry(.)%>%
  as_tibble()%>%
  dplyr::rename(.,Jan=brazil_city_diffJan)%>%
  dplyr::rename(.,Feb=brazil_city_diffFeb)%>%
  dplyr::rename(.,Mar=brazil_city_diffMar)%>%
  dplyr::rename(.,Apr=brazil_city_diffApril)%>%
  dplyr::rename(.,May=brazil_city_diffMay)%>%
  dplyr::rename(.,Jun=brazil_city_diffJune)%>%
  dplyr::rename(.,Jul=brazil_city_diffJuly)%>%
  dplyr::rename(.,Aug=brazil_city_diffAug)%>%
  dplyr::rename(.,Sep=brazil_city_diffSept)%>%
  dplyr::rename(.,Oct=brazil_city_diffOct)%>%
  dplyr::rename(.,Nov=brazil_city_diffNov)%>%
  dplyr::rename(.,Dec=brazil_city_diffDec)
city_climate_diff

tidy_city_diff <- city_climate_diff %>%
  pivot_longer(everything(), 
               names_to="Months", 
               values_to="temp_diff")
view(tidy_city_diff)

facet_plot <- tidy_city_diff %>%
  mutate(Months = factor(Months, levels = c("Jan","Feb","Mar",
                                            "Apr","May","Jun",
                                            "Jul","Aug","Sep",
                                            "Oct","Nov","Dec")))
# Plot faceted histogram
ggplot(facet_plot, aes(x=temp_diff, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = .3)+
  labs(title="Brazil", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Months ~ .)+
  theme(plot.title = element_text(hjust = 0.5))



