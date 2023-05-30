library(tidyverse)
theme_set(theme_light())

centenarians <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv')

# Plot 1

centenarians |>
  filter(still_alive == "deceased") |>
  count(place_of_death_or_residence, gender, sort = T) |>
  mutate(place_of_death_or_residence = fct_reorder(place_of_death_or_residence, n, sum)) |>
  ggplot(aes(n, place_of_death_or_residence, fill = gender)) +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "count",
       y = "place of death",
       fill = "",
       title = "The Distribution of Deceased Centenarians across Countries")

#ggsave("plot1.png", width = 7)

# Plot 2

centenarians |>
  ggplot(aes(age, fill = still_alive)) +
  geom_histogram(alpha = 0.6) +
  scale_fill_brewer(palette = "Set2") +
  theme(panel.grid = element_blank()) +
  labs(fill = "",
       title = "Centenarians Age Distribution")

#ggsave("plot2.png")

# Plot 3

centenarians |>
  filter(still_alive == "deceased") |>
  group_by(place_of_death_or_residence) |>
  summarize(min_birthdate = min(birth_date),
            max_birthdate = max(birth_date),
            min_deathdate = min(death_date),
            max_deathdate = max(death_date),
            .groups = "drop") |>
  ggplot(aes(y = place_of_death_or_residence)) +
  geom_errorbarh(aes(xmin = min_birthdate,
                     xmax = max_birthdate,
                     color = "Birth")) +
  geom_errorbarh(aes(xmin = min_deathdate,
                     xmax = max_deathdate,
                     color = "Death")) +
  theme(legend.position = "bottom") +
  labs(y = NULL,
       color = NULL,
       title = "The Minimum and Maximum Birth and Death Years Per Country") 

#ggsave("plot3.png")

# Plot 4

centenarians |>
  filter(still_alive == "deceased") |>
  group_by(gender) |>
  slice_max(age, n = 10) |>
  ungroup() |>
  mutate(name = fct_reorder(name, age)) |>
  ggplot(aes(age, name, fill = gender)) +
  geom_col(alpha = 0.6) +
  scale_fill_brewer(palette = "Set1") +
  labs(y = NULL,
       fill = NULL,
       title = "10 Longest Lived Persons") 

#ggsave("plot4.png")









