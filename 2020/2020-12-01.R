library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(zoo)
library(ggtext)
library(patchwork)

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

p <- ggplot(pup1, mapping = aes(x = occupancy_date, y = occ_7day, color = sector)) +
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
  ggtitle(
    paste0(
      "<span style = 'color:#660000'><span style = 'font-size:16pt'>**How has occupancy changed over time in Toronto homeless shelters?**</span>",
      "<br><span style = 'color:#660000'><span style = 'font-size:12pt'>and which sectors receive the most support?</span>"
    )
  ) +
  theme(plot.background = element_rect(fill = "#F9ECF0"),
        plot.title = ggtext::element_markdown(),
        panel.background = element_rect(fill = "#F9ECF0"),
        legend.background = element_rect(fill = "#F9ECF0"),
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        legend.title = element_markdown()) +
  labs(x = paste0("<span style = 'color:#660000'>**Date**</span>"),
       y = paste0("<span style = 'color:#660000'>**Occupancy (7-day average)**</span>"),
       color = paste0("<span style = 'color:#660000'>**Sector**</span>"))
p

ggsave("2020/2020-12-01.png")
