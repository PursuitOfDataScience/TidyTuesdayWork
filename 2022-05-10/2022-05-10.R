library(tidyverse)
library(lubridate)
library(ggraph)
library(tidygraph)
theme_set(theme_bw())

nyt_titles <- read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv') %>%
  mutate(title = str_to_title(title))
nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv') %>%
  mutate(title = str_to_title(title))

# Plot 1

nyt_full %>%
  group_by(title, author) %>%
  summarize(n = n(),
            min_date = min(week),
            max_date = max(week)) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  filter(year(min_date) > 1980) %>%
  head(20) %>%
  mutate(title_author = paste0(title, " (", author,")"),
         title_author = fct_reorder(title_author, min_date)) %>%
  ggplot(aes(y = title_author, color = n)) +
  geom_errorbarh(aes(xmin = min_date,
                     xmax = max_date),
                 height = 0.5,
                 size = 1) +
  geom_text(aes(label = paste(n, "weeks"), x = max_date),
            hjust = -0.1) +
  scale_color_gradient(
    high = "green",
    low = "red"
  ) +
  theme(panel.grid = element_blank(),
        legend.position = "none") +
  labs(y = NULL,
       x = NULL,
       color = "# of weeks",
       title = "The Top 20 Long-Lived Bestsellers on NYT Since 1980") 

#ggsave("plot1.png", width = 20, height = 8)

# Plot 2

nyt_full %>%
  filter(rank == 1) %>%
  group_by(title, author) %>%
  summarize(year_first = first(week),
            year_last = last(week),
            n = n()) %>% 
  arrange(desc(n)) %>%
  ungroup() %>%
  head(20) %>%
  mutate(title = fct_reorder(title, n)) %>%
  ggplot(aes(n, title, fill = author, color = author)) +
  geom_col(alpha = 0.5) +
  geom_text(aes(label = author),
            hjust = -0.05) +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  labs(x = "# of weeks",
       y = NULL,
       title = "The 20 Most Long-Lived Best Bestsellers (Rank is 1)")

#ggsave("plot2.png", width = 20, height = 8)

# Plot 3

nyt_full %>%
  distinct(title, author) %>%
  count(author, sort = T) %>%
  head(15) %>%
  inner_join(nyt_full,
             by = "author") %>%
  distinct(title, author) %>%
  as_tbl_graph() %>%
  ggraph(layout = 'fr') + 
  geom_edge_link(alpha = 0.5) +
  geom_node_point(aes(color = name)) + 
  geom_node_text(aes(label = name, color = name), hjust = 0.5, vjust = 1, check_overlap = T, size = 4) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(size = 18)) +
  labs(title = "15 Most Popular Authors with Their Works") 

#ggsave("plot3.png", width = 10, height = 8, bg = "white")

# Plot 4

nyt_titles %>%
  mutate(decade = factor(10 * (year %/% 10))) %>%
  ggplot(aes(decade, total_weeks, fill = decade, color = decade)) +
  geom_boxplot(alpha = 0.5) +
  #geom_smooth(method = "loess", aes(group = 1), se = F, alpha = 0.6) +
  #geom_jitter() +
  scale_y_log10() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(size = 18)) +
  labs(y = "total # of weeks",
       title = "Total # of Weeks on NYT Bestsellers per Decade") 

#ggsave("plot4.png", width = 10, height = 8)

