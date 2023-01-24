library(tidyverse)
library(lubridate)
library(tidytext)
theme_set(theme_light())

survivalists <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv')
loadouts <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/loadouts.csv')
episodes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/episodes.csv')
seasons <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/seasons.csv')


# Plot 1

episodes |>
  ggplot(aes(viewers, imdb_rating, color = factor(season))) +
  geom_point(alpha = 0.5, aes(size = n_ratings)) +
  geom_text(aes(label = title), check_overlap = T, hjust = 1, vjust = 1) +
  labs(x = "US viewers (million)",
       y = "IMDB rating",
       color = "season",
       size = "# of ratings",
       title = "Vewers and IMDB Rating Per Episode")

#ggsave("plot1.png", width = 8, height = 6)


# Plot 2

episodes |>
  ggplot(aes(air_date, imdb_rating, group = year(air_date), color = factor(season))) +
  geom_line() +
  geom_point(aes(size = viewers)) + 
  guides(color = "none") +
  labs(x = "air date",
       y = "IMDB rating",
       size = "viewers (million)",
       title = "Air Date and IMDB Rating")

#ggsave("plot2.png", width = 13, height = 6)

# Plot 3

survivalists |>
  ggplot(aes(age, days_lasted, color = gender)) +
  geom_point() +
  geom_smooth(se = F) +
  geom_text(aes(label = profession), check_overlap = T, vjust = 1, hjust = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(y = "days lasted",
       color = "",
       title = "Age, Gender, and Days Lasted")

#ggsave("plot3.png", width = 13, height = 6)

# Plot 4

loadouts |>
  count(season, item, sort = T)  |>
  group_by(season) |>
  slice_max(n, n = 10) |>
  ungroup() |>
  mutate(season = factor(paste("Season", season)),
         item = reorder_within(item, n, season)) |>
  ggplot(aes(n, item, fill = season)) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~season, scales = "free_y") +
  theme(legend.position = "none") +
  labs(x = "count",
       y = NULL,
       title = "Most Popular Items Per Season")

ggsave("plot4.png", width = 13, height = 8)












