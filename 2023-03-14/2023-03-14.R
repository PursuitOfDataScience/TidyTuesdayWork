library(tidyverse)
library(scales)
theme_set(theme_light())

drugs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-14/drugs.csv')

# Plot 1

drugs |>
  select(category, where(is.logical)) |>
  pivot_longer(2:9) |>
  mutate(name = str_replace_all(name, "_", " ")) |>
  add_count(category) |>
  filter(value) |>
  add_count(category, name) |>
  mutate(pct = nn/n) |>
  ggplot(aes(category, name, fill = pct)) +
  geom_tile() +
  scale_fill_gradient(labels = percent,
                      high = "darkgreen",
                      low = "lightgreen") +
  theme(panel.grid = element_blank()) +
  labs(x = NULL,
       y = NULL,
       title = "The Percentage of Drug Property Between Human and Veterinary")

#ggsave("plot1.png", width = 6)

# Plot 2

drugs |>
  count(category, authorisation_status) |>
  na.omit() |>
  ggplot(aes(category, authorisation_status, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(fill = "count",
       x = NULL,
       y = "authorization status",
       title = "Authorization Status per Category") 

#ggsave("plot2.png", width = 6)




