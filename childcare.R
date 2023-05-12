library(tidyverse)
library(tidytuesdayR)
library(ggmap)

pip <- tt_load("2023-05-09")

costs <- pip$childcare_costs
counties <- pip$counties %>%
  mutate(county_fips_code = as.factor(county_fips_code))

#looking at costs over time in counties in oregon
costs_over_time <- costs %>%
  select(county_fips_code, study_year, mcsa,
         mc_infant, mc_toddler,
         pr_f, pr_p, mhi_2018) %>%
  mutate(county_fips_code = as.factor(county_fips_code),
         year = ymd(study_year, truncated = 2L),
         mcsa = as.numeric(mcsa),
         mc_infant = as.numeric(mc_infant),
         mc_toddler = as.numeric(mc_toddler)
         ) %>%
  select(!study_year) %>%
  left_join(counties) %>%
  filter(county_name == "Multnomah County") %>%
  mutate(weekly_income = mhi_2018/52)
  
  
ggplot(costs_over_time, mapping = aes(x = year, y = mcsa)) +
  geom_col()

ggplot(costs_over_time, mapping = aes(x = year, y = mc_infant)) +
  geom_col()

ggplot(costs_over_time, mapping = aes(x = year, y = weekly_income)) +
  geom_col() +
  geom_col(inherit.aes = FALSE, mapping = aes(x = year, y = mc_infant, fill = "red"))


