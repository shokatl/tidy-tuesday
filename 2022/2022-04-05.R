library(tidyverse)
library(tidytuesdayR)

pip <- tt_load("2022-04-05")

news <- pip$news_orgs

news_sum_bystate <- news %>%
  select(state, tax_status_current, year_founded) %>%
  group_by(state, year_founded, tax_status_current) %>%
  count()

news_sum <- news %>%
  select(tax_status_current, year_founded) %>%
  mutate(tax_status_current = case_when(
    tax_status_current %in% c("For Profit") ~ "For Profit",
    tax_status_current %in% c("LLC", "Partnership", "S Corp",
                              "Public-benefit corporation") 
    ~ "LLC, Partnership, S Corp, or Pbc",
    tax_status_current %in% c("Nonprofit 501c(3) or Canadian nonprofit", 
                              "Not for Profit", "Under umbrella of a 501c(3)")
    ~ "Nonprofit/not for profit",
    TRUE ~ "Other"
  )) %>%
  group_by(year_founded, tax_status_current) %>%
  count() %>%
  ungroup() %>%
  group_by(tax_status_current) %>%
  summarize(cumulative = cumsum(n), year_founded, tax_status_current) %>%
  filter(year_founded > 1999) %>%
  drop_na()
  

ggplot(filter(news_sum, year_founded > 1999), mapping = aes(x = year_founded, 
                               y = cumulative, color = tax_status_current)) +
  geom_line()
