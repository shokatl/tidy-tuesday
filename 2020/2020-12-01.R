library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(zoo)

pip <- tt_load("2020-12-01")

pup <- pip$shelters

pup1 <- pup %>%
  #Want to get numbers grouped by sector over time
  group_by(sector, occupancy_date) %>%
  #Summarizing to get total occupancy for each sector, each day
  summarise(tot_occ = sum(occupancy), tot_cap = sum(capacity)) %>%
  #taking 7-day averages to smooth the graph
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
  theme(plot.background = element_rect(fill = "#F9ECF0"),
        panel.background = element_rect(fill = "#F9ECF0"),
        legend.background = element_rect(fill = "#F9ECF0")) +
  labs(x = "Date",
       y = "Occupancy (7-day average)",
       color = "Sector",
       title = "Occupancy of shelters in Toronto by sector over time")
