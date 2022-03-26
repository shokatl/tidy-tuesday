library(tidyverse)
library(tidytuesdayR)

#ridgeline plot package
#install.packages("ggridges")                         # Install and load ggridges package
library("ggridges")

pip <- tt_load("2022-03-22")

applicants <- pip$applicants
babynames <- pip$babynames
births <- pip$births
lifetables <- pip$lifetables

plot_babynames <- babynames %>%
  filter(name %in% c("Diane", "Rita", "Katie", "Caroline"))

ggplot(data = plot_babynames, mapping = aes(x = year, y = n)) +
  geom_point() +
  facet_wrap(~ name, ncol = 1)
