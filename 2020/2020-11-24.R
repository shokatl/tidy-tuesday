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

#getting length data into a numberical format, saving things like "round-trip" or "out and back" as new var "type"
hikes1 <- hikes %>%
  separate(length, into = c("length", "type"), sep = ",") %>%
  mutate(length = str_remove_all(length, "[:alpha:]")) %>%
  mutate(length = parse_number(length),
         rating = as.numeric(rating),
         gain = as.numeric(gain),
         highpoint = as.numeric(highpoint))%>%
  filter(rating > 4)

place <- hikes1 %>%
  select(name) %>%
  mutate_geocode(name)

hike_locations <- left_join(place, hikes1) %>%
  drop_na(lat, lon)

hike_map_box <- c(bottom = min(hike_locations$lat), left = min(hike_locations$lon), 
                  top = max(hike_locations$lat), right = max(hike_locations$lon))

hike_map <- get_stamenmap(hike_map_box, 
                          maptype = "terrain-background",
                          zoom = 5)
hike_map %>% 
  ggmap()


hike_map %>% 
  ggmap() +
  geom_point(data = hike_locations, aes(lon, lat,
                                        size = length,
                                        color = rating),
             inherit.aes = FALSE, alpha = 0.5) +
  theme_void()

hiker  <- makeIcon(
  iconUrl = "https://openclipart.org/download/11401/johnny-automatic-NPS-map-pictographs-part-95.svg",
  iconWidth = 20, iconHeight = 40)
hiker

content <- paste("<b>", hike_locations$name, 
                 "</b></br>", "Length:",
                 hike_locations$length,
                 "</br>", "Gain:",
                 hike_locations$gain,
                 "</br>", "Rating:",
                 hike_locations$rating)

leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~lon, lat = ~lat, 
             data = hike_locations, 
             popup = content, icon = hiker)
