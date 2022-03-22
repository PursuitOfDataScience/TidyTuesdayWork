library(tidyverse)
library(tidytext)
library(tidylo)
library(scales)
theme_set(theme_bw())

babynames <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv') %>%
  mutate(decade = 10 * floor(year / 10)) %>%
  rename(raw_count = n) %>%
  mutate(sex = fct_recode(sex, 
                          "Female" = "F",
                          "Male" = "M")) 

# Plot 1

babynames %>%
  group_by(sex, name, decade) %>%
  summarize(decade_name_count = sum(raw_count), .groups = "drop") %>%
  bind_log_odds(decade, name, decade_name_count) %>%
  group_by(sex, decade) %>%
  slice_max(log_odds_weighted, n = 5, with_ties = F) %>%
  ungroup() %>%
  mutate(name = reorder_within(name, log_odds_weighted, decade)) %>%
  ggplot(aes(log_odds_weighted, name, fill = sex)) +
  geom_col() +
  facet_wrap(~decade, scales = "free_y") +
  scale_y_reordered() +
  labs(x = "weighted log odds",
       y = NULL,
       fill = NULL,
       title = "Top 5 Most Distinct Names per Decade")

#ggsave("plot1.png", width = 12, height = 8)

# Plot 2

babynames %>%
  group_by(year, sex) %>%
  summarize(n = n(), .groups = "drop") %>%
  ggplot(aes(year, n, color = sex)) +
  geom_point() +
  geom_line() +
  labs(x = "",
       y = "# of name options",
       color = NULL,
       title = "# of Name Options for both Male and Female") +
  scale_x_continuous(breaks = seq(1880, 2020, 20))

#ggsave("plot2.png", width = 12, height = 8)

# Plot 3

babynames %>%
  group_by(decade, name, sex) %>%
  mutate(max_decade_prop = max(prop)) %>%
  distinct(sex, name, decade, max_decade_prop) %>%
  ungroup() %>%
  group_by(decade) %>%
  slice_max(max_decade_prop, n = 5, with_ties = F) %>%
  ungroup() %>%
  mutate(name = reorder_within(name, max_decade_prop, sex, sum)) %>%
  ggplot(aes(max_decade_prop, name, fill = factor(decade))) +
  geom_col() +
  scale_y_reordered() +
  scale_x_continuous(labels = percent) +
  facet_wrap(~sex, scales = "free_y") +
  labs(x = "maximum decade proportion of total births",
       y = NULL,
       fill = "decade",
       title = "The Most Widely Used Names per Decade")

#ggsave("plot3.png", width = 12, height = 8)

# Plot 4

gender_netural_names <- babynames %>%
  count(decade, sex, name, wt = raw_count, sort = T, name = "name_count") %>%
  group_by(decade, name) %>%
  mutate(total_name_count = sum(name_count)) %>%
  ungroup() %>%
  filter(name_count < total_name_count)

gender_netural_names %>% 
  mutate(gender_ratio = name_count/total_name_count) %>%
  distinct(decade, name, .keep_all = T) %>%
  filter(gender_ratio > 0.49,
         gender_ratio < 0.51) %>%
  group_by(decade) %>%
  slice_max(total_name_count, n = 5, with_ties = F) %>%
  ungroup() %>%
  mutate(decade = paste0("Decade: ", decade),
         name = reorder_within(name, total_name_count, decade)) %>%
  ggplot(aes(total_name_count, name, fill = factor(decade))) +
  geom_col(show.legend = F) +
  scale_y_reordered() +
  facet_wrap(~decade, scales = "free_y") +
  labs(x = "Total Name Count",
       y = "",
       title = "The Gender-Neutral Names per Decade")

#ggsave("plot4.png", width = 12, height = 8)  




