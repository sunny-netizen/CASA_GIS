library(sf)
library(here)
here()

# see inside
st_layers(here::here("prac3_data", "gadm36_AUS.gpkg"))

# READ LAYERS
Ausoutline <- st_read(here("prac3_data", "gadm36_AUS.gpkg"), 
                      layer='gadm36_AUS_0')
#check CRS
print(Ausoutline)
st_crs(Ausoutline)$proj4string

#how to add crs if none: adding the proj4string to the file or assigning an EPSG code.
#Normally if a layer has a missing CRS, it’s WGS84. But check for any metadata
Ausoutline <- Ausoutline %>%
  st_set_crs(., 4326)

Ausoutline <- st_read(here("prac3_data", "gadm36_AUS.gpkg"), 
                      layer='gadm36_AUS_0') %>% 
  st_set_crs(4326)

# REPROJECT
AusoutlinePROJECTED <- Ausoutline %>%
  st_transform(.,3112)
print(AusoutlinePROJECTED)

#From sf to sp
AusoutlineSP <- Ausoutline %>%
  as(., "Spatial")

#From sp to sf
AusoutlineSF <- AusoutlineSP %>%
  st_as_sf()

#However for generating maps in packages like leaflet, 
#your maps will need to be in WGS84, rather than a projected (flat) reference system .


# RASTER DATA
# WorldClim
library(rgdal)
library(sp)
library(raster)
jan<-raster(here("prac3_data", "wc2.1_5m_srad_01.tif"))
jan
plot(jan)

?projectRaster

#projectRaster() from the Raster package only accepts PROJ4 strings.
# set the proj 4 to a new object
newproj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# one by one method: get the jan raster and give it the new proj4
pr1 <- jan %>%
  projectRaster(., crs=newproj)
plot(pr1)

#Mollweide back to to WSG
pr1 <- pr1 %>%
  projectRaster(., crs="+init=epsg:4326")
plot(pr1)

# Data Loading
## look in our folder, find the files that end with .tif and 
library(fs)
dir_info("prac3_data/") 

#Select what we want
library(tidyverse)
listfiles<-dir_info("prac3_data/") %>%
  filter(str_detect(path, ".tif")) %>%   
  #dplyr::select(path)%>%   #don't need this step
  #pull()
  pull(path)
#pull() from dplyr which is the same as the $
listfiles

#raster stack
worldclimtemp <- listfiles %>%
  stack()
worldclimtemp

# access the january layer from stack
worldclimtemp[[1]]

# rename layers
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(worldclimtemp) <- month

# access the january layer from stack
worldclimtemp$Jan


# Extract data from raster
site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
          "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
          "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)
#Put all of this inforamtion into one list 
samples <- data.frame(site, lon, lat, row.names="site") #so site is not seen as column. only accepts xy data.
# Extract the data from the Rasterstack for all points 
AUcitytemp<- raster::extract(worldclimtemp, samples)
AUcitytemp
# Add city names  ????????
Aucitytemp2 <- AUcitytemp %>% 
  as_tibble()%>% 
  add_column(Site = site, .before = "Jan") #mutate doesnt allow specify where
Aucitytemp2


#Descriptive (or summary) statistics 
#forming the base of quantitative analysis leading to inferential statistics

#ways to subset data
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")
Perthtemp <- Aucitytemp2[3,]

#Histogram
hist(as.numeric(Perthtemp))

#define where you want the breaks in the historgram
userbreak<-c(10000,12000,14000,16000,18000,20000,22000,24000,26000,28000,30000)
hist(as.numeric(Perthtemp), 
     breaks=userbreak, 
     col="red", 
     main="Histogram of Perth Temperature", 
     xlab="Temperature", 
     ylab="Frequency")

# 
histinfo <- Perthtemp %>%
  as.numeric()%>%
  hist(.)
histinfo

#plot(Ausoutline$geom) 
#load the rmapshaper package
install.packages("rmapshaper")
library(rmapshaper)
#simplify the shapefile
#keep specifies the % of points
#to keep
AusoutSIMPLE<-Ausoutline %>%
  ms_simplify(.,keep=0.05)

plot(AusoutSIMPLE$geom)

# check if layers in same CRS
print(Ausoutline)
crs(worldclimtemp)
?crop
# crops temp data to the extent set by Ausoutline.
Austemp <- Ausoutline %>%
  crop(worldclimtemp,.)
plot(Austemp)
# alternate setup:
#... <- worldclimtemp %>%
  #crop(.,Ausoutline)
#crop vs mask? bigdata esp. crop first! 
# get raster in outline with mask
exactAus <- Austemp %>%
  mask(.,Ausoutline, na.rm=TRUE)
plot(exactAus)
hist(exactAus[[3]], col="red", main ="March sun")
#or subset with the word Mar
hist(raster::subset(exactAus, "Mar"), col="red", main ="March sun")
#Both our Austemp and exactAus are raster bricks. 
#A brick is similar to a stack except it is now stored as one file instead of a collection.

# Histogram with ggplot
library(ggplot2)
# make our raster into a data.frame to be compatible with ggplot2, using a dataframe or tibble
exactAusdf <- exactAus %>%
  as.data.frame()
# set up the basic histogram
gghist <- ggplot(exactAusdf, 
                 aes(x=Mar)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Australian March temperatures", 
       x="Sun", 
       y="Frequency")
# add a vertical line to the hisogram showing mean 
gghist + geom_vline(aes(xintercept=mean(Mar, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))

# all the months
squishdata<-exactAusdf%>%
  pivot_longer(
    cols = 1:12,
    names_to = "Month",
    values_to = "Temp"
  )
# subset 2 months
twomonths <- squishdata %>%
  # | = OR
  filter(., Month=="Jan" | Month=="Jun")
# mean 2 months
meantwomonths <- twomonths %>%
  group_by(Month) %>%
  summarise(mean=mean(Temp, na.rm=TRUE))
meantwomonths
# colours
ggplot(twomonths, aes(x=Temp, color=Month, fill=Month)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=meantwomonths, 
             aes(xintercept=mean, 
                 color=Month),
             linetype="dashed")+
  labs(title="Ggplot2 histogram of Australian Jan and Jun
       sun",
       x="sun",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# drop na's
data_complete_cases <- squishdata %>%
  drop_na()%>% 
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec")))
# Plot faceted histogram
ggplot(data_complete_cases, aes(x=Temp, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = 5)+
  labs(title="Ggplot2 faceted histogram of Australian sun", 
       x="sun",
       y="Frequency")+
  facet_grid(Month ~ .)+
  theme(plot.title = element_text(hjust = 0.5))
# + vs %>% ?????????????


# plotly sense
library(plotly)
# split the data for plotly based on month
jan <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jan")
jun <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jun")
# give axis titles
x <- list (title = "Sun")
y <- list (title = "Frequency")
# set the bin width
xbinsno<-list(start=0, end=40, size = 2.5)
# plot the histogram calling all the variables we just set
ihist<-plot_ly(alpha = 0.6) %>%
  add_histogram(x = jan$Temp,
                xbins=xbinsno, name="January") %>%
  add_histogram(x = jun$Temp,
                xbins=xbinsno, name="June") %>% 
  layout(barmode = "overlay", xaxis=x, yaxis=y)
ihist  #not showing up ?????????

# mean per month
meanofall <- squishdata %>%
  group_by(Month) %>%
  summarise(mean = mean(Temp, na.rm=TRUE))
# print the top 1
head(meanofall, n=1)
# standard deviation per month
sdofall <- squishdata %>%
  group_by(Month) %>%
  summarize(sd = sd(Temp, na.rm=TRUE))

# maximum per month
maxofall <- squishdata %>%
  group_by(Month) %>%
  summarize(max = max(Temp, na.rm=TRUE))

# minimum per month
minofall <- squishdata %>%
  group_by(Month) %>%
  summarize(min = min(Temp, na.rm=TRUE))

# Interquartlie range per month
IQRofall <- squishdata %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE))

# perhaps you want to store multiple outputs in one list..
lotsofstats <- squishdata %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE), 
            max=max(Temp, na.rm=T))

# or you want to know the mean (or some other stat) 
#for the whole year as opposed to each month...

meanwholeyear=squishdata %>%
  summarize(meanyear = mean(Temp, na.rm=TRUE))


#####Is “+” only for ggplot? We cannot use “%<%” there, must use “+”?

