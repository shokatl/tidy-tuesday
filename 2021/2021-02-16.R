library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(zoo)
library(ggtext)
library(patchwork)
library(viridis)
library(extrafont)
font_import()
loadfonts(quiet = TRUE)
library(grid)
library(pBrackets) 


pip <- tt_load("2021-02-16")

georgia_pop <- pip$georgia_pop %>%
  pivot_longer(cols = c(White, Colored), names_to = "Race", values_to = "Pop")


ggplot(data = georgia_pop, mapping = aes(x = Year, y = Pop, lty = Race)) +
  geom_line(size = 0.3) +
  coord_flip() +
  scale_linetype_manual(labels = c("= COLORED                                                             WHITE =", ""), 
                        values = c("solid", "dashed")) + 
  scale_y_reverse(n.breaks = 19, expand=c(0,0), minor_breaks = FALSE) +
  scale_x_continuous(n.breaks = 9, limits = c(1790, 1890), expand=c(0,0), minor_breaks = FALSE) +
  #scale_y_continuous(n.breaks = 19, expand=c(0,0), minor_breaks = FALSE) +
  theme(
    text = element_text(family = "Franklin Gothic Medium"),
    plot.title = element_text(hjust = 0.5),
    plot.background = element_rect(color = "#F0D6B7", fill = "#F0D6B7"),
    panel.border = element_rect(color = "black", fill = "transparent", size = .2),
    panel.grid.major = element_line(color = "red", size = .1),
    panel.background = element_rect(color = "#F0D6B7", fill = "#F0D6B7"),
    axis.title.x = element_text(size = 9, vjust = -21),
    legend.background = element_rect(color = "transparent", fill = "transparent"),
    legend.key = element_rect(color = "#F0D6B7", fill = "#F0D6B7"),
    legend.key.width = unit(2.25, "cm"),  
    legend.position = "bottom",
    legend.text.align = 0,
    legend.box.margin = margin(6, 6, 35, 0)
  ) +
  labs(
    x = "",
    y = "PERCENTS",
    lty = "",
    title = "COMPARATIVE INCREASE OF WHITE AND COLORED \nPOPULATION OF GEORGIA."
  )

grid.brackets(381.5, 512, 31.5, 512, lwd = 0.5, col="black")

ggsave("2021/2021-02-16.png", scale = 0.78)  

