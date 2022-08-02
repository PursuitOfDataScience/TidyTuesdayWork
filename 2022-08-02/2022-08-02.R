library(tidyverse)
library(lubridate)
library(widyr)
library(ggraph)
theme_set(theme_bw())

frog <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frog.csv') %>%
  janitor::clean_names() %>%
  mutate(survey_date = mdy(survey_date),
         gender = if_else(female == 1, "Female", "Male")) %>%
  select(-c(site, female, beaver)) 

# Plot 1

frog %>%
  count(type, gender, month = month(survey_date, label = T)) %>%
  mutate(type = fct_reorder(type, -n, sum)) %>%
  ggplot(aes(month, n, fill = type, group = type)) +
  geom_area(alpha = 0.7) +
  facet_grid(gender~type) +
  theme(legend.position = "none") +
  labs(y = "# of survey frogs",
       x = NULL,
       title = "# of Survey Frogs per Month")

#ggsave("plot1.png")

# Plot 2


frog %>%
  count(water, type) %>%
  mutate(water = str_to_title(water),
         water = fct_reorder(water, n),
         type = fct_reorder(type, -n, sum)) %>%
  ggplot(aes(n, water, fill = type)) +
  geom_col() +
  labs(x = "# of frogs",
       y = "",
       fill = "",
       title = "# of Frogs per Water Type")

#ggsave("plot2.png")

# Plot 3

frog %>%
  count(gender, detection) %>%
  mutate(detection = fct_reorder(detection, n, sum)) %>%
  ggplot(aes(gender, detection, fill = n)) +
  geom_tile() +
  scale_fill_gradient2(low = "red",
                       high = "green",
                       mid = "pink",
                       midpoint = 80) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "",
       y = "",
       fill = "# of frogs",
       title = "Frog Genders and Dections") 

#ggsave("plot3.png")

# Plot 4

frog %>%
  ggplot(aes(frequency, gender, fill = gender)) +
  geom_violin(show.legend = F, alpha = 0.5) +
  scale_fill_manual(values = c("green", "red")) +
  labs(y = NULL,
       title = "Male and Female Frog Frequency") 

#ggsave("plot4.png")

