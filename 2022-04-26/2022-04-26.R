library(tidyverse)
library(tidytext)
library(tidylo)
library(ggraph)
library(widyr)
theme_set(theme_bw())

hidden_gems <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-26/hidden_gems.csv') %>%
  select(-c(link_forum:notebook, author_twitter:author_linkedin))

# Plot 1

hidden_gems %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>%
  count(vol, word, sort = T) %>%
  filter(word != "fixednewmodelsexp029nonmean") %>%
  bind_log_odds(vol, word, n) %>%
  group_by(vol) %>%
  slice_max(abs(log_odds_weighted), n = 5, with_ties = F) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, log_odds_weighted, vol),
         vol = paste("Episode", vol),
         vol = fct_reorder(vol, parse_number(vol))) %>%
  ggplot(aes(log_odds_weighted, word, fill = factor(vol))) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~vol, scales = "free_y") +
  theme(legend.position = "none") +
  labs(x = "weighted log odds",
       y = "review word",
       title = "Top 5 Review Words with the Largest Weighted Log Odds")

#ggsave("plot1.png", width = 17, height = 10)

# Plot 2

set.seed(2022)

hidden_gems %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>%
  add_count(word) %>%
  filter(n > 5) %>%
  select(-n) %>%
  count(vol, word, sort= T) %>%
  pairwise_cor(word, vol, value = n, sort = T) %>%
  filter(correlation > 0) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(width = correlation, color = correlation),
                 alpha = 0.3) +
  geom_node_point() +
  geom_node_text(aes(label = name, color = name), 
                 hjust = 0.7, 
                 vjust = 1, 
                 check_overlap = T, 
                 size = 8) +
  scale_edge_width_continuous(range = c(1,3)) +
  theme_void() +
  guides(color = "none") +
  labs(edge_width = "correlation",
       title = "How are the title words correlated?") +
  theme(plot.title = element_text(size = 18))

#ggsave("plot2.png", width = 15, height = 10, bg = "white")

# Plot 3

set.seed(2022)

hidden_gems %>%
  add_count(author_kaggle) %>%
  filter(n > 1) %>%
  select(-n) %>%
  pairwise_cor(author_kaggle, vol, sort = T) %>%
  filter(correlation > 0) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(width = correlation, color = correlation),
                 alpha = 0.3) +
  geom_node_point() +
  geom_node_text(aes(label = name, color = name), 
                 hjust = 0.7, 
                 vjust = 1, 
                 check_overlap = T, 
                 size = 8) +
  scale_edge_width_continuous(range = c(1,3)) +
  theme_void() +
  guides(color = "none") +
  labs(edge_width = "correlation",
       title = "How are Kaggle authors correlated?") +
  theme(plot.title = element_text(size = 18))

#ggsave("plot3.png", width = 16, height = 10, bg = "white")

# Plot 4

hidden_gems %>%
  filter(fct_lump(author_name, 10) != "Other") %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words) %>%
  count(author_name, word, sort = T) %>%
  bind_tf_idf(word, author_name, n) %>%
  group_by(author_name) %>%
  slice_max(tf_idf, n = 5, with_ties = F) %>%
  ungroup() %>%
  mutate(word = fct_reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word, fill = author_name)) +
  geom_col() +
  facet_wrap(~author_name, scales = "free_y") +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        strip.text = element_text(size = 13),
        plot.title = element_text(size = 17)) +
  labs(x = "TF-IDF",
       y = "review word",
       title = "Top 5 Review Words with the Largest TF-IDF per Author")  

#ggsave("plot4.png", width = 13, height = 10)  



