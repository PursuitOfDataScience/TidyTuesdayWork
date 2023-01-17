library(tidyverse)
theme_set(theme_light())

artists <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv') |>
  mutate(artist_race = na_if(artist_race, "N/A"),
         artist_gender = na_if(artist_gender, "N/A"))

# Plot 1
artists |>
  na.omit() |>
  ggplot(aes(space_ratio_per_page_total, artist_race, fill = artist_gender)) +
  geom_boxplot() +
  scale_x_log10() +
  facet_wrap(~book, ncol = 1) +
  labs(x = "space ratio per page",
       y = "",
       fill = "",
       title = "Space Ratio and Race and Gender")

#ggsave("plot1.png")

# Plot 2

artists |>
  mutate(artist_race = fct_lump(artist_race, n = 2)) |>
  count(year, artist_race) |>
  na.omit() |>
  ggplot(aes(year, n, color = artist_race)) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  labs(y = "artist count",
       color = "",
       title = "Artist and Race Count Per Year")

#ggsave("plot2.png")

# Plot 3

artists |>
  count(artist_race, artist_gender) |>
  na.omit() |>
  ggplot(aes(artist_gender, artist_race, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c(trans = "log10") +
  labs(x = NULL,
       y = NULL,
       fill = "count",
       title = "Race and Gender")

#ggsave("plot3.png")















