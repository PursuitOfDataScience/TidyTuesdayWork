library(tidyverse)
theme_set(theme_light())

afrisenti <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv')
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv')
language_scripts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_scripts.csv')
language_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_countries.csv')
country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/country_regions.csv')


# join afrisenti and language_countries, country_regions together

joined <- afrisenti |>
  #select(-tweet) |>
  left_join(language_countries, by = "language_iso_code", multiple = "all") |>
  distinct() |>
  left_join(country_regions, by = "country", multiple = "all") |>
  left_join(language_scripts, by = "language_iso_code", multiple = "all") |>
  left_join(languages, by = "language_iso_code", multiple = "all") 

joined 









