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
  filter(county_name == "Deschutes County") %>%
  mutate(weekly_income = mhi_2018/52)
  
  
ggplot(costs_over_time, mapping = aes(x = year, y = mcsa)) +
  geom_col()

ggplot(costs_over_time, mapping = aes(x = year, y = mc_infant)) +
  geom_col()

#calculating education costs as a proportion of weekly income in all counties:

props_all <- costs %>%
  select(county_fips_code, study_year, mcsa,
         fme_2018, h_under6_single_m, households,
         mc_infant, mc_toddler,
         pr_f, pr_p, mhi_2018) %>%
  mutate(county_fips_code = as.factor(county_fips_code),
         mcsa = as.numeric(mcsa),
         mc_infant = as.numeric(mc_infant),
         mc_toddler = as.numeric(mc_toddler)) %>%
  left_join(counties) %>%
  mutate(weekly_income = mhi_2018/52,
         wi_fe = fme_2018/52) %>%
  mutate(prop_sa = mcsa/weekly_income,
         prop_infant = mc_infant/weekly_income,
         prop_toddler = mc_toddler/weekly_income,
         percent_single_m = (h_under6_single_m/households)*100)
  
#looking at only most recent year
props_2018 <- props_all %>%
  filter(study_year == "2018")

ggplot(props_2018, mapping = aes(x = weekly_income, y = mcsa)) +
  geom_point(color = "red", alpha = 0.3) +
  geom_point(mapping = aes(x = weekly_income, y = mc_infant), color = "blue", alpha = 0.3) +
  geom_point(mapping = aes(x = weekly_income, y = mc_toddler), color = "green", alpha = 0.3)

ggplot(props_2018, mapping = aes(x = pr_f, y = mc_infant)) +
  geom_point(color = "maroon", alpha = 0.5)

ggplot(props_2018, mapping = aes(x = weekly_income, y = mc_infant)) +
  geom_point(color = "maroon", alpha = 0.5)

ggplot(props_2018, mapping = aes(x = percent_single_m, y = fme_2018)) +
  geom_point(color = "maroon", alpha = 0.5)


