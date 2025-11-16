## Load packages
install.packages("countrycode")

library(tidyverse)
library(sf)
library(here)
library(janitor)
library(countrycode)
library(dplyr)
library(readr)
library(tmap)

#read in data
World <- st_read("C:/Users/Elliot/Desktop/05GIS/week4/homework/World_Countries_(Generalized)_-573431906301700955/World_Countries_Generalized.shp")

data2010 <- read_csv(here::here("hdr-data 2010.csv"),
                     locale = locale(encoding = "latin1"),
                     na = "n/a")
data2019 <- read_csv(here::here("hdr-data 2019.csv"),
                     locale = locale(encoding = "latin1"),
                     na = "n/a")
### Extract the required columns from the CSV and calculate the differences.
#计算差值
gii2010 <- data2010 %>%
  select(iso = countryIsoCode, country, GII_2010 = value)
gii2019 <- data2019 %>%
  select(iso = countryIsoCode, country, GII_2019 = value)
differencedata <- gii2010 %>%
  merge(
    .,
    gii2019,
    by.x="iso",
    by.y="iso"
  ) %>%
  mutate(diff_2010_2019 = GII_2019 - GII_2010)

### Transform the coordinates and extract the useful columns.
world84<- st_transform(World, 4326)
st_crs(world84)
names(world84)
world_clean <- world84 %>%
  select(iso = ISO, country = COUNTRY, geometry)
head(world_clean)

### Link the difference outcomes with shpfile
mapdata <- world_clean %>%
  merge(
    .,
    differencedata,
    by.x="country",
    by.y="country.x"
  )
### Make some adjustment, and get a static map
tmap_mode("plot")

tm_shape(mapdata) +
  tm_polygons(
    col = "diff_2010_2019",
    palette = "-RdYlBu",              
    border.col = "grey60",             
    lwd = 0.3,                         
    title = "Difference",
    showNA = FALSE
  ) +
  tm_layout(
    title = "Global Gender Inequality Index Change (2010–2019)",
    title.size = 1.2,
    title.position = c("center", "top"),
    frame = FALSE,
    legend.position = c("left", "bottom"), 
    legend.bg.color = "white",             
    legend.bg.alpha = 0.8,                 
    legend.text.size = 0.8,
    legend.title.size = 0.9,
    inner.margins = c(0.05, 0.05, 0.1, 0.1), 
    asp = 0                               
  ) +
  tm_compass(
    type = "8star",
    size = 2,
    position = c("left", "top")
  ) +
  tm_scale_bar(
    position = c("right", "bottom")
  )
### An interactive map by OSM
tmap_mode("view")

tm_shape(mapdata) + 
  tm_polygons(
    fill = "diff_2010_2019",
    palette = "Reds",
    style = "jenks",
    alpha = 0.7,
    id = "country",
    popup.vars = c(
      "Country:" = "country",
      "Data_2010:" = "GII_2010",
      "Data_2019:" = "GII_2019",
      "Diff:" = "diff_2010_2019"
    ),
    title = "Diff (2010 −> 2019)"
  ) +
  tm_basemap(server = "OpenStreetMap") +
  tm_compass(type = "arrow", position = c("left", "bottom")) + 
  tm_scalebar(position = c("left", "bottom")) +
  tm_title("Change in Gender Inequality Index (2010 -> 2019)",
           size = 2,
           position = c("center", "top"))
