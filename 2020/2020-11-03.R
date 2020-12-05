library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(zoo)
library(ggtext)
library(patchwork)

pip <- tt_load("2020-11-10")

mobile <- pip$mobile

landline <- pip$landline


ggplot(mobile, 
       mapping = aes(x = year, y = mobile_subs)) +
  geom_jitter(color = "#59B88C", alpha = 0.3) +
  geom_jitter(inherit.aes = FALSE, 
            data = landline,
            mapping = aes(x = year, y = landline_subs),
            color = "#457CBE", alpha = 0.3)



  
