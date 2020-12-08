library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(zoo)
library(ggtext)
library(patchwork)
library(viridis)

pip <- tt_load("2020-11-10")

mobile <- pip$mobile

landline <- pip$landline


p <- ggplot(mobile, 
       mapping = aes(x = year, y = mobile_subs, 
                     color = gdp_per_cap)) +
  geom_jitter(alpha = .7) +
  scale_colour_viridis(direction = -1) +
  theme_classic() +
  ggtitle(
    paste0(
      "<span style = 'color:#380474'><span style = 'font-size:16pt'>**Do countries with higher GDP have a higher proportion<br>of mobile subscriptions?**</span>"
    )
  ) +
  theme(plot.background = element_rect(fill = "#F1E6FE"),
        plot.title = ggtext::element_markdown(),
        panel.background = element_rect(fill = "#F1E6FE"),
        legend.background = element_rect(fill = "#F1E6FE"),
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown(),
        legend.title = element_markdown()) +
  labs(x = paste0("<span style = 'color:#380474'>**Year**</span>"),
       y = paste0("<span style = 'color:#380474'>**Mobile subscriptions per 100 people**</span>"),
       color = paste0("<span style = 'color:#380474'>**GDP per capita**</span>"))
  
p

ggsave("2020/2020-11-10.png", scale = 0.78)



  
