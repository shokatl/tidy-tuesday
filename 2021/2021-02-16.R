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
  theme(
    plot.background = element_rect(color = "#FAD8BA", fill = "#FAD8BA"),
    panel.background = element_rect(color = "#FAD8BA", fill = "#FAD8BA"),
    legend.background = element_rect(color = "#FAD8BA", fill = "#FAD8BA"),
    panel.border = element_rect(color = "black", fill = "transparent"),
    panel.grid.major = element_line(color = "red")
  )
