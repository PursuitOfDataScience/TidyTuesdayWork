library(tidyverse)
library(geofacet)
library(scales)
library(lubridate)
theme_set(theme_light())

tornados <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-16/tornados.csv') |>
  mutate(month = month(date, abbr = T, label = T))

# Plot 1
tornados |>
  group_by(yr, st) |>
  summarize(n = n(), .groups = "drop") |>
  ggplot(aes(yr, n, fill = st)) +
  geom_area(alpha = 0.6, show.legend = F) +
  scale_y_log10() +
  facet_geo(~st) +
  theme(axis.text.x = element_text(size = 6)) +
  labs(x = NULL,
       y = "tornado count",
       title = "Annual Tornado Count Per State")

#ggsave("plot1.png", width = 10, height = 6)

# Plot 2
tornados |>
  mutate(decade = 10 * (yr %/% 10)) |>
  group_by(decade, month) |>
  summarize(n = n()) |>
  ungroup() |>
  ggplot(aes(decade, month, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "pink",
                      high = "darkred") +
  scale_x_continuous(expand = c(0,0)) +
  theme(legend.position = "bottom") +
  labs(x = NULL,
       y = NULL,
       fill = "# of tornados",
       title = "Monthly Tornodo Count Per Decade across the U.S.")

#ggsave("plot2.png")

# Plot 3

tornados |>
  ggplot(aes(loss, month, fill = month)) +
  geom_violin(show.legend = F) +
  scale_x_log10(labels = dollar_format()) +
  labs(y = NULL,
       title = "The Loss Caused by Tornados Per Month")

#ggsave("plot3.png")

# Plot 4

tornados |>
  filter(len > 0,
         wid > 0) |>
  mutate(casualty = inj + fat) |>
  ggplot(aes(len, wid)) +
  geom_point(aes(size = casualty), alpha = 0.1) +
  geom_text(aes(label = st), check_overlap = T, size = 2) +
  scale_x_continuous(label = number) +
  labs(x = "length (miles)",
       y = "width (yards)",
       title = "Tornado Sizes and Casualty Impacts")

#ggsave("plot4.png")
















