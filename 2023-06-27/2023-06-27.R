library(tidyverse)
library(lubridate)
theme_set(theme_light())

us_place_names <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_names.csv')
us_place_history <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_history.csv')

# Plot 1

us_place_names |>
  filter(prim_long_dec < 0,
         prim_lat_dec > 0) |>
  mutate(year_created = year(date_created)) |>
  ggplot(aes(prim_long_dec, prim_lat_dec, color = year_created)) +
  geom_point(size = 0.1) +
  borders("state") +
  scale_color_gradient(low = "lightgreen",
                       high = "darkgreen") +
  theme_void() +
  labs(color = "year created",
       title = "City Name Created and Entered to Database Year Information")

#ggsave("plot1.png", bg = "white", width = 6, heigh = 4)

# Plot 2

us_place_names |>
  left_join(us_place_history, by = "feature_id") |> 
  filter(!is.na(history),
         prim_long_dec < 0,
         prim_lat_dec > 10) |> 
  mutate(origin_year = as.numeric(str_extract(history, "\\d{4}"))) |>
  filter(origin_year < 2000,
         origin_year > 1400) |>
  distinct(feature_name, state_name, county_name, origin_year, prim_lat_dec, prim_long_dec) |>
  na.omit() |>
  ggplot(aes(prim_long_dec, prim_lat_dec, color = origin_year)) +
  geom_point(size = 0.5) +
  borders("county", colour = "grey95") +
  theme_void() +
  scale_color_gradient(high = "pink",
                       low = "magenta") +
  labs(color = "origin year",
       title = "Place Origin Year")

#ggsave("plot2.png", bg = "white", width = 6, heigh = 4)  



