library(tidyverse)
library(lubridate)
theme_set(theme_light())

squirrel_data <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv') |>
  janitor::clean_names() |>
  mutate(date = mdy(date))

# Plot 1

squirrel_data |>
  filter(!is.na(primary_fur_color)) |>
  ggplot(aes(x, y, color = primary_fur_color)) +
  geom_point(alpha = 0.5) +
  labs(color = "fur color",
       title = "Central Park Squirrel Location and Fur Color") +
  theme_void() 

#ggsave("plot1.png", bg = "white")

# Plot 2

squirrel_data |>
  select(x, y, age, where(is.logical)) |>
  filter(age != "?") |>
  pivot_longer(4:16) |>
  mutate(name = str_replace_all(name, "_", " ")) |>
  group_by(age, name, value) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(age, name) |>
  mutate(n_total = sum(n)) |>
  ungroup() |>
  mutate(pct = n/n_total) |>
  ggplot(aes(age, pct, fill = value)) +
  geom_col() +
  facet_wrap(~name) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = c(0.6, 0.1)) +
  labs(x = "",
       y = "",
       fill = "",
       title = "Adult and Juvenile Squirrel Behavior Percentage") 

#ggsave("plot2.png", width = 7)

# Plot 3

squirrel_data |>
  filter(age != "?") |>
  count(date, age, location) |>
  na.omit() |>
  ggplot(aes(date, n, fill = location)) +
  geom_area(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "bottom") +
  facet_wrap(~age, ncol = 1) +
  labs(x = "",
       y = "squirrel count",
       title = "Where Were the Squirrels?")

#ggsave("plot3.png")













