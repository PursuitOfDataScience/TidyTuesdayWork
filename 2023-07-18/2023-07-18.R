library(tidyverse)
theme_set(theme_light())
detectors <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-18/detectors.csv')

# Plot 1
detectors |>
  count(kind, .pred_class, detector) |>
  ggplot(aes(kind, .pred_class, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  facet_wrap(~detector, ncol = 4) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  theme(legend.position = c(0.9, 0.15)) +
  labs(x = "truth",
       y = "prediction",
       fill = "count",
       title = "Can AI Write As Good As Human?",
       subtitle = "Faceted by detectors") 

#ggsave("plot1.png")

# Plot 2
detectors |>
  count(model, .pred_class) |>
  ggplot(aes(n, model, fill = .pred_class)) +
  geom_col() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "count",
       y = "writer",
       fill = "prediction",
       title = "Who Is the Best Writer?")

#ggsave("plot2.png")

# Plot 3
detectors |>
  filter(!is.na(native)) |>
  count(native, .pred_class) |>
  add_count(native, wt = n) |>
  mutate(pct = n / nn) |>
  ggplot(aes(native, .pred_class)) +
  geom_point(size = 30, aes(color = pct)) +
  geom_text(aes(label = paste0(100 * round(pct,2), "%"))) +
  scale_color_viridis_c(alpha = 0.8, label = scales::percent_format()) +
  labs(y = "prediction",
       color = "",
       title = "AI Prediction For Native and Non-Native English Spearkers")

#ggsave("plot3.png", width = 6)



