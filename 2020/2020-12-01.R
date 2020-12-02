library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(zoo)

pip <- tt_load("2020-12-01")

pup <- pip$shelters


ggplot(pup, mapping = aes(x = occupancy_date, y = occupancy)) +
  geom_point(alpha = 0.1) +
  geom_point(mapping = aes(x = occupancy_date, y = capacity), 
             color = "red", alpha = 0.1)

pup1 <- pup %>%
  group_by(sector, occupancy_date) %>%
  summarise(tot_occ = sum(occupancy), tot_cap = sum(capacity)) %>%
  mutate(occ_7day = rollmean(tot_occ, k = 7, fill = NA),
         cap_7day = rollmean(tot_cap, k = 7, fill = NA))

ggplot(pup1, mapping = aes(x = occupancy_date, y = occ_7day, color = sector)) +
  geom_line(linetype = "solid") + 
  geom_line(mapping = aes(x = occupancy_date, y = cap_7day, color = sector), 
            linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c("#6C5B7B", "#C06C84", 
                                "#355C7D", "#F67280",
                                "#F8B195")) +
  annotate(geom = "text",  
           x = as.POSIXct(0, origin = '2018-10-15'), y = 3520,
           label = "Capacity",
           color = "black") +
  annotate(geom = "text",  
           x = as.POSIXct(0, origin = '2019-03-15'), y = 3000,
           label = "Occupancy",
           color = "black") +
  theme_classic() +
  labs(x = "Date",
       y = "Occupancy (7-day average)",
       color = "Sector",
       title = "Occupancy of shelters in Toronto by sector over time")






#__________________________________________________________
pup_city <- pup %>%
  mutate(city = case_when(
    organization_name == "City of Toronto" ~ "Yes",
    TRUE ~ "No"
  )) %>%
  group_by(sector, occupancy_date, city) %>%
  summarise(tot_occ = sum(occupancy), tot_cap = sum(capacity)) 
  #mutate(occ_7day = rollmean(tot_occ, k = 7, fill = NA),
         #cap_7day = rollmean(tot_cap, k = 7, fill = NA))

ggplot(data = filter(pup_city, city == "Yes"), 
       mapping = aes(x = occupancy_date, 
                     y = tot_cap)) +
  geom_line(color = "#355C7D") + 
  geom_line(data = filter(pup_city, city == "No"), 
            mapping = aes(x = occupancy_date, 
                          y = tot_cap), 
            color = "#F67280") +
  facet_wrap(~sector)

pup_2017 <- pup %>%
  mutate(year = year(occupancy_date)) %>%
  filter(year == 2017) %>%
  group_by(sector, occupancy_date) %>%
  summarise(tot_occ = sum(occupancy), tot_cap = sum(capacity))

ggplot(pup_2017, mapping = aes(x = occupancy_date, y = tot_occ, color = sector)) +
  geom_line() + 
  geom_line(mapping = aes(x = occupancy_date, y = tot_cap, color = sector), 
            linetype = "dashed") +
  scale_color_manual(values = c("#6C5B7B", "#C06C84", 
                                "#355C7D", "#F67280",
                                "#F8B195"))

pup_mid2017 <- pup %>%
  mutate(year = year(occupancy_date),
         month = month(occupancy_date)) %>%
  filter(year == 2017) %>%
  filter(month %in% c(4, 5, 6)) %>%
  group_by(sector, occupancy_date) %>%
  summarise(tot_occ = sum(occupancy), tot_cap = sum(capacity))

ggplot(pup_mid2017, mapping = aes(x = occupancy_date, y = tot_occ, color = sector)) +
  geom_line() + 
  geom_line(mapping = aes(x = occupancy_date, y = tot_cap, color = sector), 
            linetype = "dashed") +
  scale_color_manual(values = c("#6C5B7B", "#C06C84", 
                                "#355C7D", "#F67280",
                                "#F8B195"))


