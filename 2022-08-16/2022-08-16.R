library(tidyverse)
theme_set(theme_bw())

characters <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/characters.csv')


# Plot 1

characters %>%
  mutate(uni_name = fct_lump(uni_name, 14),
         uni_name = fct_reorder(uni_name, notability)) %>%
  ggplot(aes(notability, uni_name, fill = uni_name, color = uni_name)) +
  geom_boxplot(show.legend = F, alpha = 0.5) +
  labs(x = "notability score",
       y = NULL,
       title = "Universes and Character Notability Scores") 

#ggsave("plot1.png")

# Plot 2

characters %>%
  group_by(uni_name) %>%
  summarize(n = n(),
            min_score = min(notability),
            mean_score = mean(notability),
            max_score = max(notability)) %>%
  ungroup() %>%
  mutate(uni_name = fct_lump(uni_name, n = 30, w = mean_score)) %>%
  filter(uni_name != "Other") %>%
  mutate(uni_name = fct_reorder(uni_name, mean_score)) %>%
  ggplot(aes(mean_score, uni_name, color = uni_name)) +
  geom_point(aes(size = n)) +
  geom_errorbarh(aes(xmin = min_score,
                     xmax = max_score)) +
  scale_color_discrete(guide = "none") +
  labs(x = "notability score",
       y = NULL,
       size = "# of characters",
       title = "30 Shows with Largest Average Notability Scores",
       subtitle = "Min, max scores on both ends of the errorbar") 

#ggsave("plot2.png")

# Plot 3

characters %>%
  arrange(notability) %>%
  head(10) %>%
  mutate(level = "low",
         notability = -notability) %>%
  bind_rows(characters %>%
              arrange(notability) %>%
              tail(10) %>%
              mutate(level = "high")) %>%
  mutate(name = fct_reorder(name, notability)) %>%
  ggplot(aes(notability, name, fill = notability > 0)) +
  geom_col(show.legend = F) +
  geom_vline(xintercept = 0, size = 1, color = "red", linetype = 2) +
  scale_x_continuous(labels = ~abs(.x), breaks = c(-25, 0, 75, 100)) +
  scale_fill_manual(values = c("pink", "lightgreen")) +
  labs(x = "notability score",
       y = "character",
       title = "The 10 Most Notable & Least Notable Characters") 

#ggsave("plot3.png")

# Plot 4

read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/myers_briggs.csv") %>% 
  ggplot(aes(number_users, avg_match_perc)) +
  geom_bin_2d(bins = 10) +
  geom_text(aes(label = char_name), color = "lightgreen", size = 2, check_overlap = T) +
  scale_x_log10() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_viridis_c() +
  labs(x = "# of users",
       y = "average match percentage",
       title = "Myers-Briggs Personality Assessment Overview") 

#ggsave("plot4.png")


