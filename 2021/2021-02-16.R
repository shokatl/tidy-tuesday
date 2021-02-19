library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(zoo)
library(ggtext)
library(patchwork)
library(viridis)
library(forcats)

pip <- tt_load("2021-02-16")

georgia_pop <- pip$georgia_pop %>%
  pivot_longer(cols = c(White, Colored), names_to = "Race", values_to = "Pop")

ggplot(data = georgia_pop, mapping = aes(x = Year, y = Pop, lty = Race)) +
  geom_line() +
  coord_flip() +
  scale_y_reverse(n.breaks = 19, expand=c(0,0), minor_breaks = FALSE) +
  scale_x_continuous(n.breaks = 9, limits = c(1790, 1890), expand=c(0,0), minor_breaks = FALSE) +
  #scale_y_continuous(n.breaks = 19, expand=c(0,0), minor_breaks = FALSE) +
  theme(
    #title = element_text(family = "URWGothic"),
    plot.background = element_rect(color = "#F0D6B7", fill = "#F0D6B7"),
    panel.background = element_rect(color = "#F0D6B7", fill = "#F0D6B7"),
    legend.background = element_rect(color = "#F0D6B7", fill = "#F0D6B7"),
    legend.key = element_rect(color = "#F0D6B7", fill = "#F0D6B7"),
    panel.border = element_rect(color = "black", fill = "transparent"),
    panel.grid.major = element_line(color = "red", size = .2),
    legend.position = "bottom"
  ) +
  labs(
    x = "",
    y = "PERCENTS",
    lty = "",
    title = "COMPARATIVE INCREASE OF WHITE AND COLORED \nPOPULATION OF GEORGIA."
  )
  

