library(tidyverse)
theme_set(theme_bw())


constructor_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructor_standings.csv')
constructors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructors.csv')
driver_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/driver_standings.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/drivers.csv')
races <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/races.csv')

# join drivers and driver_standings

driver_standings_processed <- drivers %>%
  right_join(driver_standings, by = "driverId") %>%
  unite("name", forename:surname, sep = ' ')


# first plot
driver_standings_processed %>%
  count(name, sort = T) %>%
  mutate(
    name = fct_lump(name, 40, w = n),
    name = fct_reorder(name, n)
  ) %>%
  filter(name != "Other") %>%
  ggplot(aes(n, name, fill = name)) +
  geom_col(show.legend = F) +
  labs(x = "# of races", y = NULL, title = "Top 40 Drivers Attending Most of Races")

#ggsave("Top 40 Drivers Attending Most of Races.png", width = 20, height = 10)  

# second plot
driver_standings_processed %>%
  group_by(nationality) %>%
  summarize(sum_wins = sum(wins)) %>%
  ungroup() %>%
  mutate(
    nationality = fct_reorder(nationality, sum_wins) 
  ) %>%
  ggplot(aes(sum_wins, nationality, fill = nationality)) +
  geom_col(show.legend = F) +
  labs(x = "total # of wins", y = NULL, title = "Every Nationality Total # of Wins")
#ggsave("Every Nationality Total # of Wins.png", width = 20, height = 10)  

# join constructors and constructor_standings and races
constructor_standings_processed <- constructors %>%
  right_join(constructor_standings, by = "constructorId") %>%
  left_join(races, by = "raceId")

# Plot 3

constructor_standings_processed %>%
  group_by(year, nationality) %>%
  summarize(wins = sum(wins)) %>%
  ungroup() %>%
  mutate(
    nationality = fct_lump(nationality, 5)
  ) %>%
  ggplot(aes(year, nationality, color = wins, size = wins)) +
  geom_point() +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  labs(y = NULL, color = "# of wins", size = "# of wins", title = "Top 5 Countries Yearly # of Wins")

#ggsave("Top 5 Countries Yearly # of Wins1.png", width = 20, height = 10)  

# Plot 4

constructor_standings_processed %>%
  group_by(year, nationality) %>%
  summarize(wins = sum(wins)) %>%
  ungroup() %>%
  mutate(
    nationality = fct_lump(nationality, n = 5, w = wins)
  ) %>%
  ggplot(aes(year, wins, color = nationality)) +
  geom_line(size = 1, show.legend = F) +
  geom_point(show.legend = F) +
  facet_grid(rows = vars(nationality)) +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  theme(
    strip.text = element_text(size = 12)
  ) +
  labs(y = "# of wins", title = "Top 5 Countries Yearly # of Wins")

ggsave("Top 5 Countries Yearly # of Wins2.png", width = 20, height = 10)  

# Plot 5

constructor_standings_processed %>%
  mutate(decade = 10 * floor(year/10)) %>%
  group_by(decade, nationality) %>%
  summarize(wins = sum(wins)) %>%
  ungroup() %>%
  mutate(
    nationality = fct_lump(nationality, n = 5, w = wins)
  ) %>%
  ggplot(aes(decade, wins, fill = nationality)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  labs(y = "# of wins", title = "Top 5 Countries Decade # of Wins")

#ggsave("Top 5 Countries Decade # of Wins.png", width = 20, height = 10)  

















  
  
