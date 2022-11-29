library(tidyverse)
theme_set(theme_light())

wcmatches <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv')
worldcups <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv')

# Plot 1

worldcups |>
  pivot_longer(cols = c(goals_scored:attendance), names_to = "stat") |>
  mutate(stat = str_to_title(str_replace(stat, "_", " "))) |>
  ggplot(aes(year, value, fill = stat)) +
  geom_col(show.legend = F) +
  facet_wrap(~stat, scales = "free") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = NULL,
       y = "respective value",
       title = "World Cup Historical Stats")

#ggsave("plot1.png")

# Plot 2

worldcups |>
  pivot_longer(winner:fourth) |>
  count(name, value, sort = T) |>
  complete(name, value, fill = list(n = NA)) |>
  mutate(name = factor(name, levels = c("winner", "second", "third", "fourth"))) |>
  ggplot(aes(name, value, fill = n)) +
  geom_tile() +
  scale_fill_gradient2(high = "green",
                       low = "red",
                       midpoint = 2,
                       mid = "pink") +
  labs(x = NULL,
       y = NULL,
       fill = "# of wins",
       title = "Top 4 Rankings Country Count")

#ggsave("plot2.png")

# Plot 3

wcmatches |>
  mutate(is_group = ifelse(str_detect(stage, "Group"), "Group Game", "After Group Game")) |>
  pivot_longer(cols = c(home_score:away_score)) |>
  mutate(name = str_replace(name, "_", " ")) |>
  ggplot(aes(value, name, fill = is_group)) +
  geom_violin(alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "score",
       y = NULL,
       fill = NULL,
       title = "World Cup Scores") 

#ggsave("plot3.png")

# Plot 4

wcmatches |>
  pivot_longer(c(home_score:away_score)) |>
  mutate(name = str_replace(name, "_", " ")) |>
  ggplot(aes(value, fill = name)) +
  geom_histogram(alpha = 0.6, position = "dodge", binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 10)) +
  labs(x = "score",
       fill = "",
       title = "Home & Away Score Distribution") 

#ggsave("plot4.png")



