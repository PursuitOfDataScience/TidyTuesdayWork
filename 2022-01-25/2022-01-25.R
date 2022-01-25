library(tidyverse)
library(tidytext)
ratings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv') %>%
  filter(playingtime > 0,
         yearpublished > 1970) 

details_processed <- details %>%
  mutate(boardgamecategory = str_remove_all(boardgamecategory, "\\[|\\]")) %>% 
  separate_rows(boardgamecategory, sep = ",\\s") %>%
  mutate(boardgamecategory = str_remove_all(boardgamecategory, "^'|'$"),
         boardgamecategory = str_replace_all(boardgamecategory, '\\"', "")) 

# plot 1
details_processed %>%
  mutate(boardgamecategory = fct_lump(boardgamecategory, n = 10)) %>%
  filter(boardgamecategory != "Other") %>%
  ggplot(aes(x = playingtime, y = boardgamecategory, fill = boardgamecategory)) +
  geom_violin(show.legend = F) +
  scale_x_log10(labels = function(x) round(x), n.breaks = 6, limits = c(1, 1000)) +
  labs(x = "playing time (minutes)",
       y = "category",
       title = "Board Game Category vs Playing Time") +
  theme(plot.title = element_text(size = 18),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13))

#ggsave("plot1.png", width = 10, height = 13)

# Plot 2
details_processed %>%
  mutate(boardgamecategory = fct_lump(boardgamecategory, n = 9)) %>%
  filter(boardgamecategory != "Other") %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, "[:digit:]")) %>%
  filter(!word %in% c("cards", "game", "quot", "play")) %>%
  count(word, boardgamecategory, sort = T) %>%
  bind_tf_idf(word, boardgamecategory, n) %>%
  group_by(boardgamecategory) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, boardgamecategory)) %>%
  ggplot(aes(tf_idf, word, fill = boardgamecategory)) +
  geom_col() +
  facet_wrap(~boardgamecategory, scales = "free_y") +
  scale_y_reordered() +
  theme(legend.position = "none",
        strip.text = element_text(size = 13),
        plot.title = element_text(size = 16)) +
  labs(x = "TF-IDF",
       y = NULL,
       title = "10 Words with the Largest TF-IDF Values for All Game Categories")

#ggsave("plot2.png", width = 12, height = 10)

# Plot 3

details %>%
  pivot_longer(cols = c(owned:wishing)) %>%
  group_by(name) %>%
  slice_max(value, n = 30) %>%
  ungroup() %>%
  select(primary, yearpublished, name, value, boardgamecategory) %>%
  ggplot(aes(yearpublished, value, color = name)) +
  geom_point() +
  geom_text(aes(label = primary), hjust = 1, vjust = 1, check_overlap = T) +
  scale_y_log10(label = scales::comma) +
  labs(x = "year of game published",
       y = "# of players",
       color = "board game status",
       title = "Top 30 Games Across All Board Game Status",
       subtitle = "Text refers to the game primary") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16))
   
#ggsave("plot3.png", width = 12, height = 10)

ratings_joined <- ratings %>%
  select(-c(url, thumbnail)) %>%
  inner_join(details_processed %>% select(id, primary, boardgamecategory), by = c("id", "name" = "primary"))


# plot 4

ratings_joined %>%
  mutate(boardgamecategory = fct_lump(boardgamecategory, n = 5)) %>%
  group_by(year, boardgamecategory) %>%
  summarize(across(c(average, users_rated), mean)) %>%
  ungroup() %>%
  filter(!is.na(boardgamecategory)) %>%
  ggplot(aes(x = year, y = average, color = boardgamecategory)) +
  geom_line() +
  geom_point(aes(size = users_rated)) +
  facet_wrap(~boardgamecategory) +
  scale_color_discrete(guide = "none") +
  labs(size = "average # of raters",
       x = NULL,
       y = "average rating",
       title = "The Average Ratings for the Top 6 Board Game Categories") +
  theme(strip.text = element_text(size = 15),
        plot.title = element_text(size = 18),
        axis.title = element_text(size = 13))

#ggsave("plot4.png", width = 13, height = 8)  
  
  
