library(tidyverse)
library(widyr)
library(ggraph)
library(tidytext)
theme_set(theme_light())

bob_ross <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv') |>
  select(-contains("src")) |>
  filter(season < 31) |>
  mutate(season = paste("Season", season))

# Plot 1

bob_ross |>
  pivot_longer(cols = 8:25, names_to = "color") |>
  filter(value) |>
  mutate(color = str_replace_all(color, "_", " ")) |>
  count(season, color) |>
  group_by(season) |>
  slice_max(n, n = 6, with_ties = F) |>
  ungroup() |>
  mutate(season = fct_reorder(season, parse_number(season))) |>
  ggplot(aes(n, color, fill = season)) +
  geom_col(show.legend = F) +
  facet_wrap(~season, scales = "free") +
  labs(x = "count",
       y = "",
       title = "Top 6 Most Popular Colors Per Season")

#ggsave("plot1.png", width = 15, height = 10)


# Plot 2

bob_ross |>
  mutate(season = fct_reorder(season, parse_number(season))) |>
  ggplot(aes(episode, season, fill = num_colors)) +
  geom_tile() +
  scale_x_continuous(breaks = seq(1, 14, 1), expand = c(0, 0)) +
  scale_fill_gradient(high = "darkgreen", low = "lightgreen") +
  theme(legend.position = "bottom") +
  labs(y = "",
       fill = "number of colors",
       title = "Number of Colors Per Season Per Episode")  


#ggsave("plot2.png", width = 10, height = 7)

# Plot 3

bob_ross |>
  pivot_longer(cols = 8:25, names_to = "color") |>
  filter(value) |>
  mutate(color = str_replace_all(color, "_", " ")) |>
  count(season, color) |>
  pairwise_cor(color, season, n) |>
  arrange(-correlation)  |>
  #head(100) |>
  ggraph(layout = "fr") +
  geom_edge_link(aes(color = correlation), alpha = 0.5, width = 1) +
  geom_node_point() +
  geom_node_text(aes(label = name), hjust = 0.5, vjust = 1, check_overlap = T, size = 4) +
  scale_edge_color_viridis() +
  theme_void() +
  guides(color = "none") +
  labs(edge_width = "correlation",
       title = "How Are the Colors Correlated?") +
  theme(plot.title = element_text(size = 18))


#ggsave("plot3.png", width = 10, height = 8, bg = "white")

# Plot 4

bob_ross |>
  unnest_tokens(word, painting_title) |>
  anti_join(stop_words) |>
  count(word, sort = T) |>
  head(25) |>
  mutate(word = fct_reorder(word, n)) |>
  ggplot(aes(n, word, color = word)) +
  geom_point() +
  geom_errorbarh(aes(xmin = 0,
                     xmax = n),
                 height = 0,
                 size = 1) +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  labs(x = "word count",
       y = "",
       title = "25 Most Popular Painting Title Words")

#ggsave("plot4.png")
