library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(ggtext)
library(patchwork)
library(stringr)
library(ggmap)
library(maps)
library(leaflet)

pip <- tt_load("2020-11-24")

hikes <- pip$hike_data

#getting variables into a numberical format, saving things like "round-trip" or "out and back" as new var "type"
hikes1 <- hikes %>%
  separate(length, into = c("length", "type"), sep = ",") %>%
  mutate(length = str_remove_all(length, "[:alpha:]")) %>%
  mutate(length = parse_number(length),
         rating = as.numeric(rating),
         gain = as.numeric(gain),
         highpoint = as.numeric(highpoint))%>%
  filter(rating > 4)

#using Google Maps API to pull in latitude and longitude of the hike locations
place <- hikes1 %>%
  select(name) %>%
  mutate_geocode(name)

#joining the two data frames
hike_locations <- left_join(place, hikes1) %>%
  drop_na(lat, lon)

#Bringing in hiker icon
hiker  <- makeIcon(
  iconUrl = "https://openclipart.org/download/11401/johnny-automatic-NPS-map-pictographs-part-95.svg",
  iconWidth = 20, iconHeight = 40)
hiker

#Setting the labels for the icons when you click on them
content <- paste("<b>", hike_locations$name, 
                 "</b></br>", "Length:",
                 hike_locations$length,
                 "</br>", "Gain:",
                 hike_locations$gain,
                 "</br>", "Rating:",
                 hike_locations$rating)

#making the interactive map
leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~lon, lat = ~lat, 
             data = hike_locations, 
             popup = content, icon = hiker)
