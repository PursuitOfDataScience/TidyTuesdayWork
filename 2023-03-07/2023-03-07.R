library(tidyverse)
library(ozmaps)
library(patchwork)
library(lubridate)
theme_set(theme_light())

numbats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv')

# Plot 1
ozmap_states <- ozmaps::ozmap_states

ggplot() +
  geom_sf(data = ozmap_states, aes(fill = NAME), show.legend = F, alpha = 0.5) +
  geom_point(data = numbats, aes(decimalLongitude, decimalLatitude, color = dryandra)) +
  coord_sf() +
  theme_void() +
  labs(title = "Where Are The Numbats?")

#ggsave("plot1.png", bg = 'white')


# Plot 2

p21 <- numbats |>
  count(year) |>
  na.omit() |>
  ggplot(aes(year, n)) +
  geom_line() +
  labs(x = "",
       y = "numbat counts",
       title = "Numbat Counts Per Year and Per Month")

p22 <- numbats |>
  count(month) |>
  na.omit() |>
  inner_join(tibble(month = month.abb,
                    month_num = seq(1,12))) |>
  mutate(month = fct_reorder(month, month_num)) |>
  ggplot(aes(month, n)) +
  geom_col() +
  labs(x = "",
       y = "numbat counts")

p21 / p22

#ggsave("plot2.png")

# Plot 3

numbats |>
  mutate(hour = hour(eventDate)) |>
  count(dataResourceName, hour) |>
  ggplot(aes(factor(hour), dataResourceName, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(x = "hour of the day",
       y = "",
       fill = "# of Numbats",
       title = "Data Resources Agencies and Numbat Event Hours")

ggsave("plot3.png", width = 10)  
