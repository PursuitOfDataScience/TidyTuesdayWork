library(tidyverse)
library(lubridate)
theme_set(theme_light())

ufo_sightings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')
places <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/places.csv')
day_parts_map <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/day_parts_map.csv')

# Plot 1

places |>
  filter(country == "USA") |>
  ggplot(aes(longitude, latitude, color = elevation_m)) +
  geom_point(alpha = 0.3, size = 0.1) +
  borders("state") +
  scale_color_gradient(low = "lightgreen", high = "red") +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(color = "elevation (meters)",
       title = "Where Were the UFO's Tracked?") 

#ggsave("plot1.png", bg = "white")

# Plot 2

ufo_sightings |>
  filter(!is.na(day_part),
         duration_seconds > 0) |>
  ggplot(aes(duration_seconds, day_part, color = day_part, fill = day_part)) +
  geom_boxplot(show.legend = F, alpha = 0.5) +
  scale_x_log10(labels = scales::comma) +
  labs(x = "seconds",
       y = "",
       title = "UFO Duration Seconds in Different Parts of Day")

#ggsave("plot2.png", width = 6)

# Plot 3
places |>
  ggplot(aes(population, elevation_m)) +
  geom_point(alpha = 0.1, size = 0.5) +
  geom_text(aes(label = city), 
            vjust = 1,
            hjust = 1,
            size = 2,
            check_overlap = T) +
  scale_x_log10(label = scales::comma) +
  labs(y = "elevation (meters)",
       title = "City Population and UFO Elevation")

#ggsave("plot3.png")

# Plot 4
day_parts_map |>
  mutate(sunrise = make_datetime(year(rounded_date), month(rounded_date), day(rounded_date), hour(sunrise), minute(sunrise), second(sunrise)),
         sunset = make_datetime(year(rounded_date), month(rounded_date), day(rounded_date), hour(sunset), minute(sunset), second(sunset)),
         sun_duration = sunset - sunrise,
         sun_duration = ifelse(sun_duration > 0, sun_duration, sun_duration + 24),
         month = month(rounded_date, label = T, abbr = F)) |>
  ggplot(aes(sun_duration, fill = month)) +
  geom_histogram(show.legend = F) +
  facet_wrap(~month) +
  labs(x = "sun duration",
       title = "Hours Between Sunrise and Sunset")

#ggsave("plot4.png")











