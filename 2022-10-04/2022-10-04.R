library(tidyverse)
library(widyr)
library(ggraph)
library(lubridate)
library(tidytext)
library(tidylo)
theme_set(theme_light())

product_hunt <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-04/product_hunt.csv') |>
  mutate(release_date = ymd(str_remove(release_date, " .+$")))


# Plot 1

product_hunt |>
  mutate(category_tags = str_to_lower(str_remove_all(category_tags, "[:punct:]"))) |> 
  separate_rows(category_tags, sep = " ") |>
  count(name, category_tags) |>
  filter(n > 3) |>
  pairwise_cor(category_tags, name, n) |>
  ggraph(layout = "fr") +
  geom_edge_link(aes(color = correlation > 0, width = correlation), alpha = 0.2) +
  geom_node_point() +
  geom_node_text(aes(label = name, color = name), hjust = 0.5, vjust = 1, check_overlap = T, size = 5) +
  theme_void() +
  guides(color = "none") +
  scale_edge_width_continuous(range = c(1, 2)) +
  scale_edge_color_brewer(palette = "Set1") +
  labs(title = "How are category tags correlated?") +
  theme(plot.title = element_text(size = 18))

#ggsave("plot1.png", bg = "white")

# Plot 2

product_hunt |>
  mutate(category_tags = str_to_lower(str_remove_all(category_tags, "[:punct:]"))) |>
  filter(category_tags != "") |>
  separate_rows(category_tags, sep = " ") |>
  mutate(category_tags = fct_lump(category_tags, n = 12, w = upvotes)) |>
  group_by(release_year = year(release_date), category_tags) |>
  summarize(upvotes = sum(upvotes), 
            n = n(),
            .groups = "drop") |>
  filter(category_tags != "Other") |>
  mutate(category_tags = fct_reorder(category_tags, -upvotes, sum)) |>
  ggplot(aes(release_year, upvotes, fill = category_tags)) +
  geom_area(alpha = 0.7) +
  facet_wrap(~category_tags) +
  theme(legend.position = "none") +
  labs(x = "release year",
       title = "# of Upvotes Per Year Per Category Tag")

#ggsave("plot2.png")


# Plot 3
  
product_hunt |>
  transmute(release_year = year(release_date),
            product_description) |>
  na.omit() |>
  unnest_tokens(word, product_description) |>
  anti_join(stop_words) |>
  count(release_year, word) |>
  filter(!str_detect(word, "[:digit:]")) |>
  bind_log_odds(release_year, word, n) |>
  filter(n > 5) |>
  group_by(release_year) |>
  slice_max(log_odds_weighted, n = 10, with_ties = F) |>
  ungroup() |>
  mutate(word = reorder_within(word, log_odds_weighted, release_year)) |>
  ggplot(aes(log_odds_weighted, word, fill = factor(release_year))) +
  geom_col(alpha = 0.7) +
  scale_y_reordered() +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~release_year, scales = "free_y", ncol = 4) +
  theme(legend.position = "none") +
  labs(x = "weighted log odds",
       y = NULL,
       title = "Top 10 Description Words with Largest Weighted Log Odds")

#ggsave("plot3.png")

# Plot 4

product_hunt |>
  mutate(year = year(release_date)) |>
  filter(upvotes > 0) |>
  ggplot(aes(upvotes, color = factor(year))) +
  geom_freqpoly(show.legend = F, size = 2) +
  scale_x_log10() +
  facet_wrap(~year, ncol = 4) +
  labs(x = "# of upvotes",
       title = "Upvotes Distribution Per Year")

#ggsave("plot4.png")  





























