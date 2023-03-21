library(tidyverse)
library(patchwork)
library(scales)
theme_set(theme_light())

languages <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-21/languages.csv')


p11 <- languages |>
  select(contains("github"), title) |> 
  ggplot(aes(github_language_repos, github_repo_stars, color = github_language_type)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = title), 
            check_overlap = T, 
            size = 3,
            hjust = 1,
            vjust = 1) +
  scale_x_log10(label = number) +
  scale_y_log10(label = number) +
  labs(x = "# of repos",
       y = "repo stars",
       color = "lauguage type",
       title = "GitHub Language Repos and Stars")

p12 <- languages |>
  select(title, contains("wiki")) |> 
  ggplot(aes(wikipedia_daily_page_views, wikipedia_revision_count, 
             color = wikipedia_backlinks_count)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = title), 
            size = 3,
            hjust = 1,
            vjust = 1,
            check_overlap = T) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "daily page views",
       y = "revision count",
       color = "backlinks count",
       title = "Wikipedia Page Views and Revision Counts")

# Plot 1
p11 / p12

#ggsave("plot1.png", height = 6, width = 8)

# Plot 2

languages |>
  filter(number_of_jobs > 1) |>
  ggplot(aes(number_of_users, number_of_jobs, color = appeared)) +
  geom_point(size = 1) +
  geom_text(aes(label = title), 
            check_overlap = T,
            size = 3,
            vjust = 1,
            hjust = 1) +
  scale_x_log10(label = number) +
  scale_y_log10(label = number) +
  scale_color_continuous(low = "lightgreen",
                         high = "darkgreen") +
  labs(x = "# of users",
       y = "# of jobs",
       color = "first appeared",
       title = "Language User and Job Numbers")

#ggsave("plot2.png", width = 6)

# Plot 3

languages |>
  filter(!is.na(line_comment_token)) |> 
  mutate(type = fct_lump(type, n = 5)) |>
  count(line_comment_token, type) |>
  mutate(line_comment_token = fct_reorder(line_comment_token, n, sum)) |>
  ggplot(aes(n, line_comment_token, fill = type)) +
  geom_col() +
  labs(x = "count",
       y = "line comment token",
       title = "Lauguage Line Comment Token and Count")

#ggsave("plot3.png")

# Plot 4

languages |>
  filter(appeared > 1900) |>
  mutate(type = fct_lump(type, n = 5)) |>
  count(type, appeared) |>
  ggplot(aes(appeared, n, color = type)) +
  geom_line()

#ggsave("plot4.png", bg = "white")



