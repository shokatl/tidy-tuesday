library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(zoo)
library(ggtext)
library(patchwork)
library(viridis)
library(forcats)

pip <- tt_load("2021-01-26")

plastics <- pip$plastics

plastics <- plastics %>%
  pivot_longer(cols = c(empty, hdpe, ldpe, o, pet, pp, ps, pvc), 
               names_to = "type",
               values_to = "count")


countries <- plastics %>%
  filter(country != "EMPTY") %>%
  mutate(country = str_replace_all(country, "_", " ")) %>%
  mutate(country = str_to_title(country)) %>%
  filter(parent_company == "Grand Total") %>%
  group_by(country) %>%
  drop_na(count) %>%
  summarize(sum = sum(count)) %>%
  filter(sum > 100)
  

c <- countries %>%
  arrange(sum) %>% 
  mutate(country = factor(country, levels = country))

ggplot(data = c, mapping = aes(x = country, y = sum)) +
  geom_col(color = "white", fill = "red1") +
  coord_flip() + 
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.title.x = element_text(size = 13, color = "black"),
    axis.title.y = element_text(size = 13, color = "black")
  ) +
  labs(
    x = "Country of cleanup",
    y = "Total plastic waste count"
  )

ggsave("2021/2021-01-26_1.png", scale = 0.78)

# Now let's look at the origins of all this plastic waste...

origins <- plastics %>%
  filter(country != "EMPTY") %>%
  mutate(country = str_replace_all(country, "_", " ")) %>%
  mutate(country = str_to_title(country)) %>%
  mutate(parent_company = str_to_title(parent_company)) %>%
  filter(parent_company != "Grand Total") %>%
  group_by(parent_company) %>%
  drop_na(count) %>%
  summarize(sum = sum(count)) %>%
  filter(sum > 100)

o <- origins %>%
  arrange(sum) %>% 
  mutate(parent_company = factor(parent_company, levels = parent_company))

ggplot(data = o, mapping = aes(x = parent_company, y = sum)) +
  geom_col(fill = "red1") +
  coord_flip() + 
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.title.x = element_text(size = 13, color = "black"),
    axis.title.y = element_text(size = 13, color = "black")
  ) +
  geom_text(inherit.aes = FALSE, aes(x = 140, y = 150000, label = "A lot of companies...")) +
  labs(
    x = "Parent company",
    y = "Total plastic waste count"
  )
 
ggsave("2021/2021-01-26_2.png", scale = 0.78)

#Now let's zoom in a bit more, and exclude "unbranded" and "null"

ggplot(data = filter(o, sum > 800 & parent_company != "Unbranded" & parent_company != "Null"), 
       mapping = aes(x = parent_company, y = sum)) +
  geom_col(color = "white", fill = "red1") +
  coord_flip() + 
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.title.x = element_text(size = 13, color = "black"),
    axis.title.y = element_text(size = 13, color = "black")
  ) +
  labs(
    x = "Parent company",
    y = "Total plastic waste count"
  )

ggsave("2021/2021-01-26_3.png", scale = 0.78)

