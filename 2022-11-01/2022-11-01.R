library(tidyverse)
library(lubridate)
library(tidytext)
library(tidylo)
theme_set(theme_light())

horror_movies <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-01/horror_movies.csv') 


# Plot 1

horror_movies |>
  mutate(original_language = fct_lump(str_to_upper(original_language), n = 5)) |>
  filter(popularity > 1,
         vote_average > 0,
         vote_count > 10) |>
  ggplot(aes(popularity, vote_average, color = original_language)) +
  geom_point(alpha = 0.3) +
  scale_x_log10() +
  scale_color_brewer(palette = "Set1") +
  labs(y = "average rating",
       color = "language",
       title = "Movie Popularity and Rating")

#ggsave("plot1.png")


# Plot 2

horror_movies |>
  mutate(original_language = fct_lump(str_to_upper(original_language), n = 5)) |>
  mutate(year = year(release_date)) |>
  count(year, original_language) |>
  mutate(original_language = fct_reorder(original_language, -n, sum)) |>
  ggplot(aes(year, n, color = original_language)) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set2") +
  labs(y = "# of movies",
       color = "language",
       title = "# of Movies Per Year Per Language")

#ggsave("plot2.png")

# Plot 3

horror_movies |>
  filter(!is.na(overview)) |>
  unnest_tokens(word, overview) |>
  anti_join(stop_words) |>
  count(original_language, word) |>
  filter(n > 5) |>
  bind_log_odds(original_language, word, n) |>
  filter(fct_lump(original_language, n = 6) != "Other") |>
  group_by(original_language) |>
  slice_max(log_odds_weighted, n = 10) |>
  ungroup() |>
  mutate(original_language = str_to_upper(original_language),
         word = reorder_within(word, log_odds_weighted, original_language)) |>
  ggplot(aes(log_odds_weighted, word, fill = original_language)) +
  geom_col(show.legend = F) +
  scale_y_reordered() +
  facet_wrap(~original_language, scales = "free") +
  labs(x = "weighted log odds",
       y = "overview word",
       title = "Top 10 Movie Overview Words by Weighted Log Odds") 

#ggsave("plot3.png", width = 8, height = 6)

# Plot 4

horror_movies |>
  filter(runtime > 10,
         runtime < 600) |>
  mutate(original_language = fct_lump(str_to_upper(original_language), n = 10),
         original_language = fct_reorder(original_language, runtime)) |>
  ggplot(aes(runtime, original_language, fill = original_language, color = original_language)) +
  geom_boxplot(show.legend = F, alpha = 0.5) +
  labs(x = "runtime (minutes)",
       y = "movie language",
       title = "Movie Language and Runtime")

#ggsave("plot4.png")  





