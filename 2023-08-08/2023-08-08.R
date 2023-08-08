library(tidyverse)
library(tidytext)
theme_set(theme_light())

episodes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/episodes.csv')
sauces <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/sauces.csv')
seasons <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/seasons.csv')

# Plot 1
seasons |>
  ggplot(aes(y = season)) +
  geom_point(size = 0.8,
             aes(color = episodes,
                 x = as.Date((as.numeric(original_release) + as.numeric(last_release)) / 2, origin = '1970-01-01'))) +
  geom_errorbarh(aes(xmin = original_release,
                     xmax = last_release)) +
  scale_color_continuous(low = "red",
                         high = "green") +
  theme(legend.position = "bottom") +
  labs(x = "date",
       color = "episode numbers",
       title = "Original Release and Last Release Date Per Season") 

#ggsave("plot1.png")

# Plot 2
sauces |>
  mutate(season = factor(season)) |>
  filter(scoville < 2000000) |> 
  ggplot(aes(season, scoville, fill = season, color = season)) +
  geom_boxplot(alpha = 0.5, show.legend = F) +
  scale_y_log10() +
  theme(panel.grid = element_blank()) +
  labs(title = "Which Season Has the Best Sauces?")

#ggsave("plot2.png")

# Plot 3

episodes |>
  filter(season < 21) |>
  unnest_tokens(word, title) |>
  anti_join(stop_words) |>
  count(season, word, sort = T) |>
  group_by(season) |>
  slice_max(n, n = 5, with_ties = F) |>
  ungroup() |>
  ggplot(aes(n, word, fill = factor(season))) +
  geom_col(show.legend = F) +
  facet_wrap(~season, scales = "free_y") +
  labs(x = "count",
       y = "title word",
       title = "Top 5 Popular Episode Title Words Per Season")

#ggsave("plot3.png", width = 8, bg = "white")



