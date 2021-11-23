library(tidyverse)
library(patchwork)
library(ggraph)
library(igraph)
library(glmnet)
library(tidytext)
library(Matrix)
library(broom)

directors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/directors.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')
writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/writers.csv')
imdb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/imdb.csv')

# join directors, episodes and writers together

joined <- episodes %>%
  left_join(directors, by = "story_number") %>%
  left_join(writers, by = "story_number") %>%
  mutate(season_number = factor(paste0("Season ", season_number), levels = paste0("Season ", unique(sort(season_number)))))
# 
# joined %>%
#   ggplot(aes(first_aired, uk_viewers)) +
#   geom_line() +
#   geom_point()



p11 <- joined %>%
  filter(season_number != "Season 13") %>%
  ggplot(aes(episode_number, uk_viewers, color = season_number))+
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~season_number) +
  theme(legend.position = "none",
        strip.text = element_text(size = 15),
        plot.title = element_text(size = 18)) +
  scale_x_continuous(breaks = seq(1,15, 2)) +
  labs(x = "episode number",
       y = "UK viewers (in millions)",
       title = "Which Season/Episode is the Most Popular One?")

p12 <- joined %>%
  filter(season_number != "Season 13") %>%
  ggplot(aes(episode_number, rating, color = season_number))+
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~season_number) +
  theme(legend.position = "none",
        strip.text = element_text(size = 15),
        plot.title = element_text(size = 18)) +
  scale_x_continuous(breaks = seq(1,15, 2)) +
  labs(x = "episode number",
       y = "rating",
       title = "Which Season/Episode has the Highest Rating?")


p13 <- joined %>%
  filter(season_number != "Season 13") %>%
  ggplot(aes(rating, uk_viewers, color = season_number)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", aes(group = 1)) +
  labs(y = "UK viewers (in millions)",
       color = "",
       title = "The Correlation between # of Viewers & Rating") +
  theme(strip.text = element_text(size = 15),
        plot.title = element_text(size = 18))

## Plot 1

(p11 / p12) | p13

#ggsave("plot1.png", width = 17, height = 15)



## Plot 2

joined %>%
  count(director, writer, sort = T, name = '# of times working together') %>%
  graph_from_data_frame() %>%
  ggraph() +
  geom_edge_link(aes(alpha = `# of times working together`)) +
  geom_node_point() +
  geom_node_text(aes(label = name),alpha = 0.5, vjust = 1, hjust = 0.7) +
  theme_void() +
  coord_flip() +
  labs(title = "How are Directors & Writers Linked?") 

#ggsave("plot2.png", width = 18, height = 13)


## Plot 3

### Lasso Model

word_matrix <- imdb %>%
  mutate(row_id = row_number()) %>%
  unnest_tokens(word, desc) %>%
  anti_join(stop_words) %>%
  add_count(word) %>%
  #filter(n > 5) %>%
  cast_sparse(row_id, word)

row_id <- as.integer(rownames(word_matrix)) 

rating <- imdb$rating[row_id]

cv_glmnet_model <- cv.glmnet(word_matrix, rating)

cv_glmnet_model$glmnet.fit %>%
  tidy() %>% 
  filter(term != "(Intercept)",
         !str_detect(term, "[:digit:]")) %>% 
  group_by(term) %>%
  slice_max(step, n = 1) %>%
  ungroup() %>%
  mutate(positive = if_else(estimate > 0, TRUE, FALSE)) %>%
  group_by(positive) %>%
  slice_max(abs(estimate), n = 20) %>% 
  ungroup() %>%
  mutate(term = fct_reorder(term, estimate)) %>%
  ggplot(aes(estimate, term, fill = term)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "Lasso estimate",
       y = "description word",
       title = "How does Description Word Impact Rating?")

#ggsave("plot3.png", width = 16, height = 11)

















