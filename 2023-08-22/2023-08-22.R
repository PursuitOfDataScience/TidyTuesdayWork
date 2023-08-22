library(tidyverse)
library(tidytext)
theme_set(theme_light())

population <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-22/population.csv')

# Plot 1
population |>
  filter(refugees > 0,
         asylum_seekers > 0,
         year > 2010) |>
  ggplot(aes(refugees, asylum_seekers)) +
  geom_point(alpha = 0.2, size = 0.5) +
  geom_smooth(method = "loess", color = "magenta") +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~year) +
  labs(y = "asylum seekers",
       title = "Refugees and Asylum Seekers")

#ggsave("plot1.png")

# Plot 2
population |>
  group_by(year) |>
  summarize(refugees = sum(refugees),
            asylum_seekers = sum(asylum_seekers),
            .groups = "drop") |>
  rename("asylum seekers" = 3) |>
  pivot_longer(cols = c(2:3)) |>
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2010, 2022, 3)) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "",
       y = "",
       color = "",
       title = "Global Refugees and Asylum Seekers Per Year")

#ggsave("plot2.png")

# Plot 3
population |>
  filter(year > 2010) |>
  group_by(year) |>
  slice_max(refugees, n = 10, with_ties = F) |>
  ungroup() |>
  mutate(coo_name = reorder_within(coo_name, refugees, year)) |>
  ggplot(aes(refugees, coo_name, fill = coo_name)) +
  geom_col(show.legend = F) +
  scale_y_reordered() +
  facet_wrap(~year, scales = "free_y") +
  labs(y = NULL,
       title = "Top Refugee Countries")

#ggsave("plot3.png", width = 12, bg = "white")

# Plot 4
population |>
  filter(refugees > 0) |>
  mutate(asylum_rate = asylum_seekers / refugees) |>
  filter(asylum_rate < 1) |> 
  ggplot(aes(year, asylum_rate, group = year, fill = factor(year), color = factor(year))) +
  geom_boxplot(show.legend = F, alpha = 0.3) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "",
       y = "asylum rate",
       title = "Asylum Rates Per Year")

#ggsave("plot4.png")







