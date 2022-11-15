library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
theme_set(theme_light())

clean_data <- function(data) {
  data |>
    mutate(date = ymd(date)) |>
    select(-timestamp)
}

image_alt <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/image_alt.csv') |>
  clean_data() |>
  mutate(measure = "Image Alt")

color_contrast <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/color_contrast.csv') |>
  clean_data() |>
  mutate(measure = "Color Contrast")

ally_scores <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/ally_scores.csv') |>
  clean_data() |>
  mutate(measure = "a11y scores")

bytes_total <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/bytes_total.csv') |>
  clean_data() |>
  mutate(measure = "Total Bytes")

speed_index <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-15/speed_index.csv') |>
  clean_data() |>
  mutate(measure = "Speed Index")

# Plot 1

image_alt |>
  bind_rows(color_contrast) |>
  ggplot(aes(date, percent, color = client)) +
  geom_line(alpha = 0.6) +
  geom_point(alpha = 0.6) +
  facet_wrap(~measure) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_color_brewer(palette = "Set1") +
  labs(x = NULL,
       y = NULL,
       color = NULL,
       title = "Percentage of Different Measures for Desktop and Mobile")

#ggsave("plot1.png")


# Plot 2


p21 <- bytes_total |>
  ggplot(aes(x = date, y = p50, fill = client, color = client)) +
  geom_ribbon(aes(ymin = p10,
                  ymax = p90),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = p25,
                  ymax = p75),
              alpha = 0.4) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  guides(color = "none") +
  labs(x = NULL,
       y = "total bytes",
       fill = NULL,
       title = "Mobile and Desktop Total Bytes",
       subtitle = "Lines stand for medians, and the range is defined by percentiles (10, 25, 75, 90).")


p22 <- speed_index |>
  ggplot(aes(x = date, y = p50, fill = client, color = client)) +
  geom_ribbon(aes(ymin = p10,
                  ymax = p90),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = p25,
                  ymax = p75),
              alpha = 0.4) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  guides(color = "none") +
  labs(x = NULL,
       y = "speed index",
       fill = NULL,
       title = "Mobile and Desktop Speed Index",
       subtitle = "Lines stand for medians, and the range is defined by percentiles (10, 25, 75, 90).")

p21 / p22

#ggsave("plot2.png", height = 8, width = 10)

# Plot 3

ally_scores |>
  ggplot(aes(x = date, y = p50, fill = client, color = client)) +
  geom_ribbon(aes(ymin = p10,
                  ymax = p90),
              alpha = 0.2) +
  geom_ribbon(aes(ymin = p25,
                  ymax = p75),
              alpha = 0.4) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  guides(color = "none") +
  labs(x = NULL,
       y = "a11y score",
       fill = NULL,
       title = "Mobile and Desktop A11Y Scores",
       subtitle = "Lines stand for medians, and the range is defined by percentiles (10, 25, 75, 90).")

#ggsave("plot3.png", height = 8, width = 10)

