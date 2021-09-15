library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_bw())

billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')%>%
  mutate(
    week_id = mdy(week_id),
    year = year(week_id)
  )

p11 <- billboard %>%
  mutate(year = year(week_id)) %>%
  group_by(year) %>%
  summarize(num_of_distinct_performer = n_distinct(performer)) %>%
  ggplot(aes(year, num_of_distinct_performer)) +
  geom_line(size = 1) +
  geom_point(aes(color = factor(year))) +
  expand_limits(y = 0) +
  labs(y = "# of distinct performer", 
       title = "# of distinct performer over the years") +
  theme(
    legend.position = "none"
  ) +
  scale_x_continuous(n.breaks = 6)
 
p12 <- billboard %>%
  mutate(decade = 10 * floor(year(week_id)/10)) %>%
  group_by(decade) %>%
  summarize(num_of_distinct_performer = n_distinct(performer)) %>%
  ggplot(aes(decade, num_of_distinct_performer)) +
  geom_line(size = 1) +
  geom_point(aes(color = factor(decade))) +
  expand_limits(y = 0) +
  labs(y = "# of distinct performer", 
       title = "# of distinct performer over the decades") +
  theme(
    legend.position = "none"
  ) +
  scale_x_continuous(breaks = seq(1950, 2020, by = 10))

# plot 1
p11 / p12

#ggsave("1.png", width = 20, height = 10) 

p21 <- billboard %>%
  mutate(week_jump = week_position - previous_week_position) %>%
  group_by(song) %>%
  summarize(max_week_jump = max(week_jump, na.rm = TRUE)) %>%
  arrange(desc(max_week_jump)) %>%
  head(50) %>%
  mutate(song = fct_reorder(song, max_week_jump)) %>%
  ggplot(aes(max_week_jump, song, fill = song)) +
  geom_col(show.legend = F) +
  labs(x = "max week jump", title = "Top 50 Songs with Maximum Week Jump")

p22 <- billboard %>%
  mutate(week_drop = previous_week_position - week_position) %>%
  group_by(song) %>%
  summarize(max_week_drop = max(week_drop, na.rm = TRUE)) %>%
  arrange(desc(max_week_drop)) %>%
  head(50) %>%
  mutate(song = fct_reorder(song, max_week_drop)) %>%
  ggplot(aes(max_week_drop, song, fill = song)) +
  geom_col(show.legend = F) +
  labs(x = "max week drop", title = "Top 50 Songs with Maximum Week Drop")

# plot 2

p21 / p22  

#ggsave("2.png", width = 20, height = 13) 

# plot 3

billboard %>%
  mutate(performer = fct_lump(performer, 12),
         performer = fct_reorder(performer, year, min)) %>%
  filter(performer != "Other") %>%
  count(year, performer, sort = T) %>%
  ggplot(aes(year, n, fill = performer)) +
  geom_area() +
  facet_wrap(~performer, scales = "free_y") +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold")
  ) +
  labs(y = "# of weeks on billboard top 100",
       title = "Top 12 Perfomers on Billboard")
  
#ggsave("3.png", width = 20, height = 13)   
  
  

