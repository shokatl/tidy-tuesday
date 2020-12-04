library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(zoo)
library(ggtext)
library(patchwork)
library(stringr)

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


