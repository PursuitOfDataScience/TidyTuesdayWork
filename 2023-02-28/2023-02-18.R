library(tidyverse)
theme_set(theme_light())

afrisenti <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv')
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv')
language_scripts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_scripts.csv')
language_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_countries.csv')
country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/country_regions.csv')


# join afrisenti and language_countries, country_regions together

joined <- afrisenti |>
  left_join(language_countries, by = "language_iso_code", multiple = "all") |>
  distinct() |>
  left_join(country_regions, by = "country", multiple = "all") |>
  left_join(language_scripts, by = "language_iso_code", multiple = "all") |>
  left_join(languages, by = "language_iso_code", multiple = "all") 

# Plot 1

joined |>
  count(language, country, sort = T) |>
  ggplot(aes(country, language, fill = n)) +
  geom_tile() +
  scale_fill_gradient(high = "darkgreen",
                      low = "lightgreen") +
  labs(fill = "tweet count",
       title = "Country VS Language")

#ggsave("plot1.png", width = 11)

# Plot 2

joined |>
  count(region, language, sort = T) |>
  mutate(region = fct_reorder(region, n, sum)) |>
  ggplot(aes(n, region, fill = language)) +
  geom_col() +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "tweet count",
       y = "",
       title = "Language Diversity Across Africa")

#ggsave("plot2.png", width = 11, height = 8)

# Plot 3

joined |>
  count(language, label) |>
  group_by(language) |>
  mutate(total = sum(n),
         pct = n/total) |>
  ungroup() |>
  ggplot(aes(pct, language, fill = label)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "tweet pct",
       y = "",
       fill = "sentiment",
       title = "Tweet Sentiment Per Language")

#ggsave("plot3.png")












