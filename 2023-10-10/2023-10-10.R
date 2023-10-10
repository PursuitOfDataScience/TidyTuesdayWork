library(tidyverse)
library(patchwork)
theme_set(theme_light())

haunted_places <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-10/haunted_places.csv') |>
  filter(longitude < -10)

# Plot 1
haunted_places |>
  ggplot(aes(longitude, latitude, color = state)) +
  geom_point(size = 0.1, alpha = 0.3, show.legend = F) +
  borders("state") +
  theme_void() +
  labs(title = "Haunted Place Locations")

#ggsave("plot1.png", bg = "white")

# Plot 2
IL <- map_data("county", "illinois") |>
  tibble() |>
  ggplot() +
  geom_polygon(aes(long, lat, group = group),
               fill = "grey70") +
  geom_point(data = haunted_places |>
               filter(state_abbrev == "IL",
                      longitude > -95,
                      latitude > 36),
             aes(longitude, latitude),
             color = "red", alpha = 0.5, size = 0.5) +
  coord_map() +
  theme_void() +
  labs(title = "Haunted Places in IL",
       subtitle = "Land of Lincoln")

OH <- map_data("county", "ohio") |>
  tibble() |>
  ggplot() +
  geom_polygon(aes(long, lat, group = group),
               fill = "grey70") +
  geom_point(data = haunted_places |>
               filter(state_abbrev == "OH",
                      longitude > -85,
                      latitude < 43),
             aes(longitude, latitude),
             color = "darkgreen", alpha = 0.5, size = 0.5) +
  coord_map() +
  theme_void() +
  labs(title = "Haunted Places in OH",
       subtitle = "Birthplace of Aviation")

IL + OH

#ggsave("plot2.png", width = 6)


# Plot 3
haunted_places |>
  count(state_abbrev, sort = T) |>
  slice_max(n, n = 6) |>
  mutate(cat = "high") |>
  bind_rows(haunted_places |>
              count(state_abbrev, sort = T) |>
              slice_min(n, n = 6) |>
              mutate(cat = "low",
                     n = -n)) |>
  mutate(state_abbrev = fct_reorder(state_abbrev, n)) |>
  ggplot(aes(n, state_abbrev, fill = cat)) +
  geom_col(show.legend = F, alpha = 0.9) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "count",
       y = "",
       title = "The Most Haunted and Least Haunted States")

#ggsave("plot3.png")


