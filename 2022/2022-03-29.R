library(tidyverse)
library(tidytuesdayR)

pip <- tt_load("2022-03-29")


sports <- pip$sports

sports_revenue <- sports %>%
  select(year, institution_name, sports,
         rev_men, rev_women) %>%
  pivot_longer(cols = c("rev_men", "rev_women"), 
               names_to = "gender",
               values_to = "revenue") %>%
  select(year, institution_name, sports, gender, revenue) %>%
  mutate(gender = str_remove(gender, "rev_"))

sports_expenditure <- sports %>%
  select(year, institution_name, sports, 
         exp_men, exp_women) %>%
  pivot_longer(cols = c("exp_men", "exp_women"), 
               names_to = "gender",
               values_to = "expenditure") %>%
  select(year, institution_name, sports, gender, expenditure) %>%
  mutate(gender = str_remove(gender, "exp_"))

sports_count <- sports %>%
  select(year, institution_name, sports,
         partic_men, partic_women) %>%
  pivot_longer(cols = c("partic_men", "partic_women"), 
               names_to = "gender",
               values_to = "count") %>%
  select(year, institution_name, sports, gender, count) %>%
  mutate(gender = str_remove(gender, "partic_"))

scr <- left_join(sports_revenue, sports_expenditure) %>%
  distinct(year, sports, institution_name, gender, .keep_all = TRUE)

sports_count_revexp <- left_join(sports_count, scr) %>%
  distinct(year, sports, institution_name, gender, 
           revenue, expenditure, .keep_all = TRUE)
  

spend <- sports_count_revexp %>%
  pivot_wider(names_from = gender,
              values_from = count) %>%
  filter(men != "NA") %>%
  filter(women != "NA") %>%
  pivot_longer(cols = c("men", "women"),
               names_to = "gender",
               values_to = "count") %>%
  left_join(y = rev_exp, by = c("year", "institution_name",
                   "sports", "gender"))


sports_count_revexp %>%
  dplyr::group_by(year, institution_name, sports, revenue, expenditure, gender) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L)


by_school <- spend %>%
  filter(sports)
  group_by(institution_name, gender) %>%
  summarize(sum_exp = sum(expenditure),
            sum_rev = sum(revenue),
            sum_count = sum(count))

ggplot(data = spend, mapping = aes(x = ))





#Overall scatterplot to show general shape of data

ggplot(rev_exp, mapping = aes(x = revenue, y = expenditure, 
                              color = gender)) +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) +
  geom_point(data = filter(rev_exp, sports == "Football"), 
             mapping = aes(x = revenue, y = expenditure),
             color = "orange")


# all sports except football
no_football <- rev_exp %>%
  filter(sports != "Football")
ggplot(no_football, mapping = aes(x = revenue, y = expenditure, 
                                 color = gender)) +
  geom_point()

