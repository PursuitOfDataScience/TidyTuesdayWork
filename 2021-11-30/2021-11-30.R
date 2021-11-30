library(tidyverse)
library(lubridate)
library(tidytext)
matches <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv") %>%
  mutate(home_team = if_else(team1_away_or_home == "away", team2, team1),
         team1_home = if_else(team1_away_or_home == "away", "Guest", "Home"),
         team2_home = if_else(team2_home_away == "away", "Guest", "Home"),
         match_date = mdy(match_date))%>%
  mutate(year = year(match_date)) %>%
  mutate(winner = str_remove(winner, "\\(.+\\)$"))

match_reshaped <- bind_rows(
  matches %>%
    select(team1, score_team1, team1_home, winner, match_date) %>%
    rename(team = "team1", 
           score_team = "score_team1",
           home_or_guest = "team1_home"),
  matches %>%
    select(team2, score_team2, team2_home, winner, match_date) %>%
    rename(team = "team2", 
           score_team = "score_team2",
           home_or_guest = "team2_home")
) 


# Plot 1
match_reshaped %>%
  ggplot(aes(score_team, team, fill = team)) +
  geom_boxplot(show.legend = F) +
  geom_vline(xintercept = 248.5, color = "red", linetype = "dotted", size = 1) +
  facet_wrap(~home_or_guest, ncol = 1) +
  theme(strip.text = element_text(size = 15),
        plot.title = element_text(size = 18)) +
  labs(x = "score",
       y = NULL,
       title = "How much does each team score when home and away?",
       subtitle = "The vertical dashed line represents the largest median score")

#ggsave("plot1.png", width = 17, height = 13)


# Plot 2

matches %>%
  count(year, winner, sort = T) %>% 
  filter(!is.na(year),
         !str_detect(winner, "Match tied")) %>%
  mutate(winner = reorder_within(winner, n, year)) %>%
  ggplot(aes(n, winner, fill = winner)) +
  geom_col(show.legend = F) +
  scale_y_reordered() +
  facet_wrap(~year, scales = "free_y") +
  labs(x = "# of wins",
       y = NULL,
       title = "# of wins") +
  theme(strip.text = element_text(size = 15),
        plot.title = element_text(size = 18))

#ggsave("plot2.png", width = 17, height = 13)

## Plot 3

matches %>%
  mutate(total_score = score_team1 + score_team2) %>%
  group_by(series, year) %>%
  summarize(avg_score = mean(total_score)) %>%
  ungroup() %>%
  filter(!is.na(year)) %>%
  mutate(series = reorder_within(series, avg_score, year)) %>%
  ggplot(aes(avg_score, series, fill = series)) +
  geom_col(show.legend = F) +
  facet_wrap(~year, scales = "free_y") +
  scale_y_reordered() +
  labs(x = "mean total score",
       y = NULL,
       title = "Which series score the most on average?") +
  theme(strip.text = element_text(size = 15),
        plot.title = element_text(size = 18))

#ggsave("plot3.png", width = 17, height = 13)

## Plot 4

matches %>%
  ggplot(aes(score_team1, score_team2, color = winner)) +
  geom_point() +
  geom_text(aes(label = player_of_match), hjust = 1, vjust = 1, check_overlap = T) +
  theme(legend.position = "none") +
  labs(x = "team 1 score",
       y = "team 2 score",
       title = "Score distribution along with the name of the player of match") 

#ggsave("plot4.png", width = 17, height = 13)



