library(tidyverse)
library(tidytext)
library(lubridate)
theme_set(theme_light())

transitions <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/transitions.csv')
timezones <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/timezones.csv')
timezone_countries <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/timezone_countries.csv')
countries <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/countries.csv')




# Plot 1
timezones |>
  inner_join(transitions |>
               filter(offset > 0) |>
               group_by(zone) |>
               summarize(mean_offset = mean(offset)) |>
               ungroup(),
             by = "zone") |>
  ggplot() +
  geom_point(aes(longitude, latitude, color = mean_offset)) +
  geom_polygon(data = map_data("world") |>
                 tibble() |>
                 filter(lat < 80 & lat > -80,
                        long < 180 & long > -180),
               aes(long, lat, group = group),
               alpha = 0.5,
               fill = "orange") +
  scale_color_continuous(low = "lightgreen",
                         high = "darkgreen") +
  coord_map() +
  theme_void() +
  labs(color = "time offset on average",
       title = "Time Offset (in seconds)") 

ggsave("plot1.png", width = 10, heigh = 6, bg = "white")

# Plot 2

transitions |>
  filter(str_detect(abbreviation, ".+T$"),
         offset > 0) |>
  mutate(abbreviation = fct_lump(abbreviation, 20),
         abbreviation = fct_reorder(abbreviation, offset)) |>
  ggplot(aes(offset, abbreviation, fill = abbreviation)) +
  geom_boxplot(show.legend = F) +
  labs(x = "offset (seconds)",
       title = "Time Zones and Offests")

#ggsave("plot2.png")

# Plot 3

timezone_countries |>
  separate(zone, into = c("continent", "country"), sep = "/") |>
  count(continent, country, sort = T) |>
  filter(n > 1,
         !continent %in% c("Indian", "Pacific")) |>
  mutate(country = str_replace(country, "_", " "),
         country = reorder_within(country, n, continent)) |>
  ggplot(aes(n, country, fill = continent)) +
  geom_col(show.legend = F) +
  scale_y_reordered() +
  facet_wrap(~continent, scales = "free") +
  labs(x = "# of countries",
       y = NULL,
       title = "Timezone and Country")

#ggsave("plot3.png")

# Plot 4

transitions |>
  filter(!str_detect(begin, "^-.+"),
         !str_detect(end, "^32767.+")) |>
  mutate(begin = ymd(str_extract(begin, "\\d{4}-\\d{2}-\\d{2}")),
         end = ymd(str_extract(end, "\\d{4}-\\d{2}-\\d{2}"))) |>
  group_by(zone) |>
  summarize(begin = min(begin),
            end = max(end)) |>
  ungroup() |>
  mutate(diff = end - begin) |>
  arrange(diff) |>
  head(30) |>
  mutate(zone = fct_reorder(zone, begin)) |>
  ggplot(aes(y = zone)) +
  geom_errorbarh(aes(xmin = begin,
                     xmax = end),
                 height = 0.5) +
  labs(x = "begin to end",
       title = "Timezone Begin and End Dates")

ggsave("plot4.png", width = 8)




  



