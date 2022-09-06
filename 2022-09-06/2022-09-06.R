library(tidyverse)
theme_set(theme_light())

sets <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz') |>
  filter(num_parts > 0)

# Plot 1

sets |>
  group_by(year) |>
  summarize(n = n(),
            min_parts = min(num_parts),
            mean_parts = mean(num_parts),
            max_parts = max(num_parts),
            .groups = "drop") |>
  ggplot(aes(year, mean_parts)) +
  geom_area(alpha = 0.5, fill = "midnightblue") +
  geom_point(aes(size = n), color = "midnightblue") +
  scale_size_continuous(range = c(1,3)) +
  labs(x = NULL,
       y = "average # of parts",
       size = "# of iventory",
       title = "Lego Sets")

#ggsave("plot1.png")

# Plot 2

sets |>
  group_by(name) |>
  summarize(n = n(),
            min_year = min(year),
            max_year = max(year),
            .groups = "drop") |>
  filter(n > 10) |>
  mutate(name = fct_reorder(name, min_year)) |>
  ggplot(aes(y = name, color = name)) +
  geom_errorbarh(aes(xmin = min_year,
                     xmax = max_year)) +
  theme(legend.position = "none") +
  labs(y = NULL,
       title = "Which Lego Set Lasted Longest?",
       subtitle = "Only Lego sets appeared more than 10 times chosen.")

#ggsave("plot2.png", height = 8, width = 10)

# Plot 3

sets |>
  add_count(name) |>
  filter(n > 13) |>
  mutate(name = fct_reorder(name, num_parts)) |>
  ggplot(aes(num_parts, name, fill = name, color = name)) +
  geom_boxplot(alpha = 0.5) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(x = "# of parts",
       y = NULL,
       title = "# of Parts Per Most Popular Lego Sets")

#ggsave("plot3.png")




