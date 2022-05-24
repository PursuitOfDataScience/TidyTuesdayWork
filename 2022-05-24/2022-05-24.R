library(tidyverse)
library(widyr)
library(ggraph)
library(tidygraph)
theme_set(theme_bw())

sevens <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/sevens.csv') %>%
  mutate(score_1 = as.numeric(score_1),
         score_2 = as.numeric(score_2))
fifteens <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/fifteens.csv')


# Plot 1

sevens %>%
  mutate(tournament = fct_lump(tournament, n = 10),
         winner = if_else(str_detect(winner, "New Zealand"), "New Zealand", winner)) %>%
  filter(stage == "Final") %>%
  count(tournament, winner, sort = T) %>%
  filter(n > 1) %>%
  mutate(winner = fct_reorder(winner, n, sum),
         tournament = fct_reorder(tournament, -n, sum)) %>%
  ggplot(aes(n, winner, fill = tournament)) +
  geom_col() +
  theme(panel.grid = element_blank()) +
  labs(x = "# of Champs",
       y = "champion",
       title = "Champions per Tournament")

#ggsave("plot1.png", width = 10, height = 8)


# Plot 2

sevens %>%
  filter(!is.na(stage),
         score_1 > 0,
         score_2 > 0) %>%
  ggplot(aes(score_1, score_2)) +
  geom_point(alpha = 0.6, aes(color = stage == "Final")) +
  geom_text(aes(label = winner),
            check_overlap = T,
            hjust = 1,
            vjust = 1,
            size = 3) +
  theme(panel.grid = element_blank()) +
  labs(x = "score 1",
       y = "score 2",
       title = "Rugby Scores and Winners")

#ggsave("plot2.png", width = 10, height = 8)

# Plot 3

fifteens %>% 
  mutate(winner = fct_lump(winner, n = 10),
         winner = fct_reorder(winner, margin_of_victory, na.rm = T)) %>%
  ggplot(aes(margin_of_victory, winner, fill = winner, color = winner)) +
  geom_boxplot(alpha = 0.4) +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  labs(x = "margin of victory",
       y = "winner",
       title = "Top 10 Winners and Margins of Victory") 

#ggsave("plot3.png", width = 10, height = 8)

# Plot 4

set.seed(2022)
fifteens %>%
  mutate(venue = str_remove(venue, ",.+$")) %>%
  count(tournament, venue, sort = T) %>%
  filter(n > 5) %>%
  pairwise_cor(venue, tournament, value = n) %>%
  arrange(desc(correlation)) %>%
  head(600) %>%
  as_tbl_graph() %>%
  ggraph(layout = 'fr') + 
  geom_edge_link(alpha = 0.5) +
  geom_node_point(aes(color = name)) + 
  geom_node_text(aes(label = name, color = name), hjust = 0.5, vjust = 1, check_overlap = T, size = 4) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(size = 18)) +
  labs(title = "How are the venues correlated within tournament") 

#ggsave("plot4.png", width = 10, height = 8, bg = "white")








