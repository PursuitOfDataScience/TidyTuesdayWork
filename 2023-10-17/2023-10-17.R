library(tidyverse)
theme_set(theme_light())

taylor_album_songs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_album_songs.csv') |>
  mutate(album_name = str_remove_all(album_name, "\\s.+$"))
taylor_all_songs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_all_songs.csv')
taylor_albums <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-17/taylor_albums.csv') |>
  na.omit() |>
  mutate(album_name = str_remove_all(album_name, "\\s\\(.+$"))

# Plot 1
taylor_albums |>
  ggplot(aes(metacritic_score, user_score, color = factor(lubridate::year(album_release)))) +
  geom_point() +
  geom_text(aes(label = album_name), check_overlap = T, vjust = 1) +
  labs(x = "metacritic score",
       y = "user score",
       color = "year",
       title = "Taylor Swift Ablum Scores") 

#ggsave("plot1.png", width = 7)

# Plot 2
taylor_album_songs |>
  select(-time_signature) |>
  pivot_longer(danceability:duration_ms, names_to = "feature") |>
  mutate(feature = str_replace(feature, "_", " ")) |>
  ggplot(aes(value, fill = feature)) +
  geom_histogram(show.legend = F, alpha = 0.8) +
  facet_wrap(~feature, scales = "free") +
  labs(title = "Taylor Swift Song Features")

#ggsave("plot2.png", width = 10, height = 6)

# Plot 3
taylor_album_songs |>
  mutate(album_name = fct_reorder(album_name, energy)) |>
  ggplot(aes(energy, album_name, fill = album_name, color = album_name)) +
  geom_boxplot(show.legend = F, alpha = 0.6) +
  labs(y = "",
       title = "Which Album Is The Most Energic One?")

#ggsave("plot3.png")

# Plot 4
taylor_albums |>
  ggplot(aes(album_release, user_score)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = album_name), check_overlap = T, size = 3, 
            vjust = 1,
            hjust = 1) +
  labs(x = "album release date",
       y = "user score",
       title = "Album Release Timeline and User Scores")

#ggsave("plot4.png")









