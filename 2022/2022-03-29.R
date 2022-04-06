library(tidyverse)
library(tidytuesdayR)

pip <- tt_load("2022-03-29")


sports <- pip$sports

#tidying revenue data
sports_revenue <- sports %>%
  select(year, institution_name, sports,
         rev_men, rev_women) %>%
  pivot_longer(cols = c("rev_men", "rev_women"), 
               names_to = "gender",
               values_to = "revenue") %>%
  select(year, institution_name, sports, gender, revenue) %>%
  mutate(gender = str_remove(gender, "rev_"))

#tidying expenditure data
sports_expenditure <- sports %>%
  select(year, institution_name, sports, 
         exp_men, exp_women) %>%
  pivot_longer(cols = c("exp_men", "exp_women"), 
               names_to = "gender",
               values_to = "expenditure") %>%
  select(year, institution_name, sports, gender, expenditure) %>%
  mutate(gender = str_remove(gender, "exp_"))

#tidying gender participation data
sports_count <- sports %>%
  select(year, institution_name, sports,
         partic_men, partic_women) %>%
  pivot_longer(cols = c("partic_men", "partic_women"), 
               names_to = "gender",
               values_to = "count") %>%
  select(year, institution_name, sports, gender, count) %>%
  mutate(gender = str_remove(gender, "partic_"))

#joining revenue and expenditure data, some rows with missing data get lost
scr <- left_join(sports_revenue, sports_expenditure) %>%
  distinct(year, sports, institution_name, gender, .keep_all = TRUE)

#joining participation data with rev/exp data
sports_count_revexp <- left_join(sports_count, scr) %>%
  distinct(year, sports, institution_name, gender, 
           revenue, expenditure, .keep_all = TRUE)
  
#ok - one more tidying step. I want to only get sports that have both a 
#men's and a women's team, not co-ed sports. I've already excluded co-ed sports
#but now I need to pivot wider with count, to get men's and women's counts in 
#the same row, and then drop rows that have NA's either in mens count or women's 
#count column, and then use these year/sport/school combos to do a join and keep
#only those rows from sports_count_revexp
sporty <- sports_count_revexp %>%
  pivot_wider(id_cols = c(year, institution_name, sports),
              names_from = gender,
              values_from = count) %>%
  drop_na(men, women) %>%
  pivot_longer(cols = c(men, women),
               names_to = "gender",
               values_to = "count")

#ok! now join, and we have what we need to calculate per-athlete revenue + expenditure
#and compare men's and women's in sports where there is both a men's and a women's team at same uni
sporty_joined <- left_join(sporty, sports_count_revexp) %>%
  mutate(per_athlete_rev = revenue/count,
         per_athlete_exp = expenditure/count) %>%
  group_by(institution_name, sports, gender) %>%
  summarize(avg_exp = mean(per_athlete_exp),
            avg_rev = mean(per_athlete_rev)) %>%
  filter(!str_detect(sports, "Track"))


#Look how much more money basketball makes or spends than any other sport
#of course, football is not in here!!
ggplot(data = sporty_joined, mapping = aes(x = avg_rev, y = avg_exp,
                                             color = gender)) +
  geom_point() +
  facet_wrap(~sports)

#now let's look without basketball:

ggplot(data = filter(sporty_joined, sports != "Basketball"), mapping = aes(x = avg_rev, y = avg_exp,
                                           color = gender)) +
  geom_point() +
  facet_wrap(~sports)




#SOCCER
soccer_spending <- sporty_joined %>%
  filter(sports == "Soccer")

ggplot(data = soccer_spending, mapping = aes(x = avg_rev, y = avg_exp,
                                             color = gender)) +
  geom_point()



#BASKETBALL
bball_spending <- sporty_joined %>%
  filter(sports == "Basketball")

ggplot(data = bball_spending, mapping = aes(x = avg_rev, y = avg_exp,
                                             color = gender)) +
  geom_point()

#TENNIS
tennis_spending <- sporty_joined %>%
  filter(sports == "Tennis")

ggplot(data = tennis_spending, mapping = aes(x = avg_rev, y = avg_exp,
                                            color = gender)) +
  geom_point()

  