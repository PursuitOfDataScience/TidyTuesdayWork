library(tidyverse)
theme_set(theme_light())

age_gaps <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv') |>
  mutate(same_gender = ifelse(character_1_gender == character_2_gender, "same gender", "different genders"))


# Plot 1

age_gaps |>
  ggplot(aes(release_year, age_difference, color = same_gender)) +
  geom_point(alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "release year",
       y = "age difference",
       color = "",
       title = "Hollywood Release Year and Age Difference")

#ggsave("plot1.png")

# Plot 2

age_gaps |>
  mutate(decade = 10 * release_year %/% 10) |>
  count(decade, character_1_gender, character_2_gender, sort = T) |>
  ggplot(aes(character_1_gender, character_2_gender, fill = n)) +
  geom_tile() +
  scale_fill_gradient(high = "darkgreen",
                      low = "lightgreen",
                      trans = "log10") +
  facet_wrap(~decade, ncol = 5) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(x = "gender of #1 character",
       y = "gender of #2 character",
       fill = "count",
       title = "The Distribution of #1 and #2 Characters Per Decade") 

#ggsave("plot2.png")

# Plot 3

age_gaps |>
  mutate(director = fct_lump(director, n = 15),
         director = fct_reorder(director, age_difference)) |>
  ggplot(aes(age_difference, director, fill = director)) +
  geom_boxplot(show.legend = F, alpha = 0.5) +
  labs(x = "age difference",
       title = "Directors and Age Differences")

#ggsave("plot3.png")

# Plot 4

age_gaps |>
  mutate(release_decade = (10 * release_year %/% 10)) |>
  group_by(release_decade) |>
  mutate(across(c(actor_1_age:actor_2_age), list(mean = mean, sd = sd))) |>
  select(release_decade, contains("mean"), contains("sd")) |>
  distinct() |>
  ungroup() |>
  ggplot(aes(y = release_decade)) +
  geom_point(aes(x = actor_1_age_mean, color = "actor 1")) +
  geom_errorbarh(aes(xmin = actor_1_age_mean - 1.96 * actor_1_age_sd,
                     xmax = actor_1_age_mean + 1.96 * actor_1_age_sd, color = "actor 1"),
                 alpha = 0.3,
                 height = 5,
                 size = 1) +
  geom_point(aes(x = actor_2_age_mean, color = "actor 2")) +
  geom_errorbarh(aes(xmin = actor_2_age_mean - 1.96 * actor_2_age_sd,
                     xmax = actor_2_age_mean + 1.96 * actor_2_age_sd, color = "actor 2"),
                 alpha = 0.3,
                 height = 5,
                 size = 1) +
  scale_color_brewer(palette = "Set1") +
  theme(panel.grid = element_blank()) +
  labs(x = "age",
       y = "release decade",
       color = NULL,
       title = "Actors 1 and 2 Age Mean and 95% CI Per Decade")

#ggsave("plot4.png")






