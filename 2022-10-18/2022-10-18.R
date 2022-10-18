library(tidyverse)
library(tidytext)
library(widyr)
library(ggraph)
theme_set(theme_light())

episodes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/episodes.csv') |>
  left_join(read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv") |>
              count(season, episode, name = "total_lines"),
            by = c("season", "episode")) |>
  mutate(season = factor(season),
         episode = factor(episode))


lines <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv") |>
  select(season, episode, line, raw_text)


# Plot 1

episodes |>
  ggplot(aes(season, total_lines, fill = episode)) +
  geom_col(position = "dodge", alpha = 0.7) +
  labs(x = "season",
       y = "total # of lines",
       title = "Stranger Things Episodes and Total Lines")

#ggsave("plot1.png")


# Plot 2

set.seed(2022)

lines |>
  unnest_tokens(word, raw_text) |>
  anti_join(stop_words) |>
  filter(!str_detect(word, "[:digit:]")) |>
  group_by(season, word) |>
  summarize(n = n(),
            .groups = "drop") |>
  filter(n > 25) |>
  pairwise_cor(word, season, n, sort = T) |>
  head(500) |>
  ggraph(layout = "fr") +
  geom_edge_link(color = "purple", alpha = 0.3) +
  geom_node_point() +
  geom_node_text(aes(label = name, color = name), hjust = 0.5, vjust = 1, check_overlap = T, size = 5) +
  theme_void() +
  guides(color = "none") +
  labs(title = "How Are Line Words Correlate Per Season?") +
  theme(plot.title = element_text(size = 18))


#ggsave("plot2.png", bg = "white", width = 10, height = 8)

# Plot 3

lines |>
  unnest_tokens(word, raw_text) |>
  anti_join(stop_words) |>
  filter(!str_detect(word, "[:digit:]")) |>
  group_by(season, word) |>
  summarize(n = n(),
            .groups = "drop") |>
  filter(n > 10) |>
  bind_tf_idf(word, season, n = n) |>
  group_by(season) |>
  slice_max(tf_idf, n = 10) |>
  ungroup() |>
  mutate(season = paste0("Season ", season),
         word = reorder_within(word, tf_idf, season)) |>
  ggplot(aes(tf_idf, word, fill = factor(season))) +
  geom_col(show.legend = F) +
  scale_y_reordered() +
  facet_wrap(~season, scales = "free_y") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "TF-IDF",
       y = "",
       title = "Top 10 Line Words with Largest TF-IDF per Season")

#ggsave("plot3.png",width = 10, height = 8)

# Plot 4

set.seed(2022)
episodes |> 
  count(directed_by, written_by) |>
  ggraph(layout = "fr") +
  geom_edge_link(aes(width = n), color = "purple", alpha = 0.3) +
  geom_node_point() +
  geom_edge_loop(aes(width = n), color = "purple", alpha = 0.3) + 
  geom_node_text(aes(label = name, color = name), hjust = 0.5, vjust = 1, check_overlap = T, size = 5) +
  theme_void() +
  scale_edge_width_continuous(range = c(1,3)) +
  guides(color = "none") +
  labs(title = "Directors and Play Writers Working Together",
       edge_width = "co-working # of times") +
  theme(plot.title = element_text(size = 18))

#ggsave("plot4.png", bg = "white", width = 10, height = 8)










