library(tidyverse)
theme_set(theme_light())

yarn <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv')

# Plot 1

yarn |>
  filter(!is.na(machine_washable)) |>
  ggplot(aes(rating_average, rating_count, color = machine_washable)) +
  geom_point(alpha = 0.1) +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_brewer(palette = "Set1") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = "bottom") +
  labs(x = "rating average",
       y = "rating count",
       color = "machine washable?",
       title = "Yarn Rating Average and Count")


#ggsave("plot1.png")


# Plot 2

yarn |>
  mutate(yarn_company_name = fct_lump(yarn_company_name, n = 20)) |>
  group_by(yarn_company_name) |>
  summarize(min_gauge = min(min_gauge, na.rm = T),
            max_gauge = max(max_gauge, na.rm = T),
            n = n(),
            .groups = "drop") |>
  filter(yarn_company_name != "Other") |>
  mutate(mid = (min_gauge + max_gauge) / 2,
         yarn_company_name = fct_reorder(yarn_company_name, min_gauge)) |>
  ggplot(aes(x = mid, y = yarn_company_name, color = yarn_company_name)) +
  geom_point(aes(size = n), alpha = 0.5) +
  geom_errorbarh(aes(xmin = min_gauge,
                     xmax = max_gauge),
                 height = 0.5) + 
  guides(color = "none") +
  theme(legend.position = "bottom") +
  labs(x = "gauge",
       y = "yarn company",
       size = "# of yarn products") 

#ggsave("plot2.png")


# Plot 3

yarn |>
  arrange(desc(rating_total)) |>
  head(1000) |>
  mutate(popularity = "1000 Most Popular") |>
  bind_rows(yarn |>
              arrange(rating_total) |>
              head(1000) |>
              mutate(popularity = "1000 Least Popular")) |>
  select(where(is.numeric), popularity) |>
  pivot_longer(cols = 1:13) |>
  filter(name != "id") |>
  ggplot(aes(value, color = popularity)) +
  geom_freqpoly(size = 1, alpha = 0.7) +
  facet_wrap(~name, scales = "free") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  labs(title = "Specs Comparison Between The Most Popular and The Least Popular Yarns",
       subtitle = "Popularity is defined by the total rating")

#ggsave("plot3.png", height = 6, width = 8)

# Plot 4

yarn |>
  filter(!is.na(texture_clean),
         rating_total > 0) |>
  mutate(texture_clean = str_remove_all(texture_clean, ",.+$"),
         texture_clean = ifelse(texture_clean == "single", "singles", texture_clean)) |>
  filter(fct_lump(texture_clean, n = 10) != "Other") |>
  mutate(texture_clean = fct_reorder(texture_clean, rating_total, na.rm = T)) |>
  ggplot(aes(rating_total, texture_clean, fill = texture_clean, color = texture_clean)) +
  geom_boxplot(show.legend = F, alpha = 0.5) +
  scale_x_log10(label = scales::comma) +
  labs(x = "total # of rating",
       y = "texture",
       title = "Top 10 Popular Textures and Their # of Ratings")

#ggsave("plot4.png")











