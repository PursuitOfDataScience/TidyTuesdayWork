library(tidyverse)
library(geofacet)
library(tidytext)
theme_set(theme_light())

state_stations <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/state_stations.csv') |>
  mutate(state = str_replace(state, "_", " "))

# plot 1

# state_stations |>
#   count(state, format) |>
#   group_by(state) |>
#   slice_max(n, n = 10) |>
#   ungroup() |>
#   ggplot(aes(n, format, fill = format)) +
#   geom_col(show.legend = F) +
#   facet_geo(~state, scales = "free")

state_stations |>
  filter(fct_lump(format, n = 8) != "Other") |>
  count(state, format) |>
  ggplot(aes(n, format, fill = format)) +
  geom_col(show.legend = F) +
  facet_geo(~state, scales = "free_x") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "# of stations",
       y = "",
       title = "8 Most Popular Stations Across States")

#ggsave("plot1.png", width = 12, height = 10)


# plot 2

state_stations |>
  count(frequency, sort = T) |>
  mutate(fm = ifelse(str_detect(frequency, "FM"), "FM", "AM")) |>
  group_by(fm) |>
  slice_max(n, n = 10) |>
  ungroup() |>
  mutate(frequency = fct_reorder(frequency, n)) |>
  ggplot(aes(n, frequency, color = fm)) +
  geom_point() +
  geom_errorbarh(aes(xmin = 0,
                     xmax = n),
                 height = 0) +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Set1") +
  labs(x = "count",
       y = "",
       title = "Top 10 Most Used AM and FM Frequencies")

#ggsave("plot2.png")

# plot 3

state_stations |>
  count(state, city, format) |>
  add_count(state, city, wt = n, name = "city_total_stations") |>
  filter(city_total_stations > 30) |>
  mutate(pct = n / city_total_stations) |>
  group_by(city) |>
  slice_max(pct, n = 5, with_ties = F) |>
  ungroup() |>
  mutate(format = reorder_within(format, pct, city)) |>
  ggplot(aes(pct, format, fill = city)) +
  geom_col(show.legend = F) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_reordered() +
  facet_wrap(~city, scales = "free_y", ncol = 5) +
  labs(x = "format percentage",
       y = "",
       title = "Top 5 Radio Formats in Cities with More Than 30 Stations")

#ggsave("plot3.png", width = 15, height = 10)

# plot 4

state_stations |>
  mutate(frequency = ifelse(str_detect(frequency, "FM"), "FM", "AM")) |>
  count(licensee, frequency, sort = T) |>
  filter(fct_lump(licensee, n = 20, w = n) != "Other") |>
  mutate(licensee = fct_reorder(licensee, n, sum)) |>
  ggplot(aes(n, licensee, fill = frequency)) +
  geom_col() +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "# of stations",
       fill = NULL,
       title = "20 Most Popular Licenses")

#ggsave("plot4.png")

