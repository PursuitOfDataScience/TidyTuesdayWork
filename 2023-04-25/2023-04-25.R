library(tidyverse)
library(janitor)
theme_set(theme_light())

winners <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/winners.csv') |>
  clean_names()
london_marathon <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/london_marathon.csv') |>
  clean_names()

london_marathon |>
  mutate(rejected = applicants - accepted) |>
  pivot_longer(cols = c(rejected, accepted)) |>
  ggplot(aes(year, value, fill = name)) +
  geom_area(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(label = scales::number) +
  theme(legend.position = "bottom",
        panel.grid = element_blank()) +
  labs(x = NULL,
       y = "count",
       fill = NULL,
       title = "London Marathon Applicants Status Per Year")

#ggsave("Plot1.png")

# Plot 2

london_marathon |>
  mutate(finish_rate = finishers/starters) |> 
  ggplot(aes(year, finish_rate)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "# of finishers/ # of starters",
       title = "London Marathon Finish Rate Per Year",
       subtitle = "Something must have happened in 2020!") 

#ggsave("Plot2.png")

# Plot 3

winners |>
  count(category, nationality) |>
  mutate(nationality = fct_reorder(nationality, n, sum)) |>
  ggplot(aes(n, nationality, fill = category)) +
  geom_col() +
  theme(legend.position = "bottom") +
  labs(x = "# of winners",
       y = NULL,
       fill = NULL,
       title = "Nationality Breakdown for Winners")

#ggsave("Plot3.png", width = 6)

# Plot 4

winners |>
  mutate(category = fct_reorder(category, -time, last)) |>
  ggplot(aes(year, time, color = category)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "bottom") +
  labs(x = NULL,
       title = "Winner Time Per Category Per Year")

ggsave("Plot4.png", width = 6)














