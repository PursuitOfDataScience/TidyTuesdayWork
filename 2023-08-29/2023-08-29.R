library(tidyverse)
library(tidytext)
theme_set(theme_light())

fair_use_cases <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-29/fair_use_cases.csv') |>
  filter(year > 1900)
fair_use_findings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-29/fair_use_findings.csv') |>
  filter(year > 1900) |>
  mutate(outcome = str_to_lower(outcome),
         outcome = str_remove_all(outcome, "[,;//.].*$"))

# Plot 1
fair_use_cases |>
  count(year, jurisdiction, sort = T) |>
  ggplot(aes(year, n, fill = jurisdiction)) +
  geom_area(alpha = 0.8) +
  labs(x = "",
       y = "case count",
       title = "Yearly Case Count Per Court")

#ggsave("plot1.png", width = 8, height = 6)

# Plot 2
fair_use_cases |>
  separate_rows(categories, sep = ";|,|/") |>
  mutate(categories = str_to_title(str_remove(categories, "^\\s"))) |>
  count(categories, fair_use_found, sort = T) |>
  ggplot(aes(fair_use_found, categories, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(x = "fair use found",
       y = "",
       fill = "count",
       title = "Fair Use Found and Categories")

#ggsave("plot2.png", width = 8, height = 6)

# Plot 3
fair_use_findings |>
  unnest_tokens(word, holding) |>
  anti_join(stop_words) |>
  filter(!str_detect(word, "[0-9]")) |>
  count(word, outcome) |>
  bind_tf_idf(word, outcome, n) |>
  group_by(outcome) |>
  slice_max(tf_idf, n = 5, with_ties = F) |>
  ungroup() |>
  mutate(word = reorder_within(word, tf_idf, outcome)) |>
  ggplot(aes(tf_idf, word, fill = outcome)) +
  geom_col(show.legend = F) +
  scale_y_reordered() +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~outcome, scales = "free") +
  labs(x = "TF-IDF",
       y = "",
       title = "Top 5 TF-IDF Words Per Category")

#ggsave("plot3.png", width = 8, height = 6)











