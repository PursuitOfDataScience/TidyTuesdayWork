library(tidyverse)
library(lubridate)
library(tidytext)
library(ggthemes)
theme_set(theme_tufte())

ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv') %>%
  mutate(runner = str_to_title(runner))
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv') %>%
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(!year %in% c(2012, 2013),
         distance > 0)

## join two tibbles together
joined_tbl <- ultra_rankings %>%
  right_join(race, by = "race_year_id")


# Plot 1
race %>%
  group_by(year, country) %>%
  summarize(total_participants = sum(participants, na.rm = T)) %>% 
  filter(total_participants > 100) %>% 
  ungroup() %>%
  mutate(country = reorder_within(country, total_participants, year)) %>%
  ggplot(aes(total_participants, country, fill = country)) +
  geom_col(show.legend = F) +
  facet_wrap(~year, scales = "free") +
  scale_y_reordered() +
  theme(
    strip.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 18)
  ) +
  labs(x = "total participants",
       y = NULL,
       title = "Yearly Hosting Country Total Participant Count",
       subtitle = "Only total participants more than 100 included")

#ggsave("p1.png", width = 17, height = 13) 

# Plot 2
race %>%
  filter(participants > 0) %>%
  group_by(year) %>%
  slice_max(participants, n = 10) %>%
  ungroup() %>%
  mutate(event = reorder_within(event, participants, year)) %>%
  ggplot(aes(participants, event, fill = country)) +
  geom_col() +
  facet_wrap(~year, scales = "free_y") +
  scale_y_reordered()+
  theme(
    strip.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 18)
  ) +
  labs(x = "participants",
       title = "The Yearly Most Popular Events")

#ggsave("p2.png", width = 17, height = 13) 

# Plot 3

joined_tbl %>%
  mutate(time = hms(as.character(time))) %>% 
  mutate(duration = hour(time) + minute(time)/60 + second(time)/3600) %>% 
  mutate(speed = distance/duration) %>%
  filter(!is.na(speed),
         !is.na(gender)) %>%
  group_by(gender) %>%
  slice_max(speed, n = 30) %>%
  ungroup() %>%
  filter(runner != "No Participants") %>%
  mutate(gender = fct_recode(gender, "Male" = "M", "Female" = "W"),
         runner = reorder_within(runner, speed, gender)) %>%
  ggplot(aes(speed, runner, fill = nationality)) +
  geom_col() +
  facet_wrap(~gender, scales = "free_y") +
  scale_y_reordered() +
  theme(
    strip.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 18)
  ) +
  labs(x = "speed (km/hour)",
       title = "The 30 Fastest Male & Female Runners' Speed")

#ggsave("p3.png", width = 17, height = 13) 

# Plot 4

ultra_rankings %>%
  group_by(race_year_id) %>%
  slice_min(rank, n = 10) %>%
  ungroup() %>% 
  filter(rank <= 10) %>% 
  mutate(rank = factor(rank),
         gender = fct_recode(gender, "Male" = "M", "Female" = "W")) %>%
  filter(!is.na(gender),
         age > 0) %>%
  ggplot(aes(age, rank, fill = rank)) +
  geom_violin(show.legend = F) +
  facet_wrap(~gender) +
  theme(
    strip.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 18)
  ) +
  labs(title = "The Relations between Rank and Age Facted by Gender") 

ggsave("p4.png", width = 17, height = 13) 


