library(tidyverse)
library(lubridate)
library(patchwork)
library(corrr)
theme_set(theme_light())

soccer <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv') |>
  mutate(Date = dmy(Date))


# Plot 1

soccer |>
  pivot_longer(cols = c("HS":"AR"), names_to = "stats") |> 
  mutate(group = str_extract(stats, ".$")) |> 
  ggplot(aes(value, fill = group)) +
  geom_histogram(show.legend = F, alpha = 0.6, position = "dodge") +
  facet_wrap(~stats, scales = "free") +
  labs(x = NULL,
       title = "Soccer Stats Distribution")

#ggsave("plot1.png", width = 8, height = 6)

# Plot 2

p21 <- soccer |>
  ggplot(aes(HS, FTHG)) +
  geom_jitter(color = "lightgreen") +
  geom_text(aes(label = HomeTeam), 
            check_overlap = T, 
            hjust = 1, 
            vjust = 1,
            size = 3) +
  labs(x = "Home Team Shots",
       y = "Final Time Home Team Goals",
       title = "Does # of Shots Help Goals?")
  
p22 <- soccer |>
  ggplot(aes(AS, FTAG)) +
  geom_jitter(color = "lightblue") +
  geom_text(aes(label = AwayTeam), 
            check_overlap = T, 
            hjust = 1, 
            vjust = 1,
            size = 3) +
  labs(x = "Away Team Shots",
       y = "Final Time Away Team Goals")

p21 + p22

#ggsave("plot2.png", width = 8, height = 6)


# Plot 3

soccer |>
  mutate(diff = FTHG - FTAG) |>
  ggplot(aes(HomeTeam, AwayTeam, fill = diff)) +
  geom_tile() +
  geom_text(aes(label = diff)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "home team",
       y = "away team",
       fill = "goal diff",
       title = "Goal Difference between Home and Away Teams")

#ggsave("plot3.png", width = 6, height = 6)


# Plot 4

soccer |>
  select(where(is.numeric)) |>
  correlate() |>
  network_plot(min_cor = .2, legend = "range") +
  ggtitle("Soccer Stats Correlation")

ggsave("plot4.png", width = 8, height = 6, bg = "white")

