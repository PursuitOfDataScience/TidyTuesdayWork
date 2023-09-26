library(tidyverse)
library(patchwork)
theme_set(theme_light())

richmondway <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-26/richmondway.csv')

# plot 1

richmondway |>
  ggplot(aes(F_count_RK, Imdb_rating, color = factor(Season))) +
  geom_point() +
  labs(x = "f count",
       y = "imdb rating",
       color = "season",
       title = "IMDB Rating and F Count Per Season")

#ggsave("plot1.png")

# Plot 2

p21 <- richmondway |>
  ggplot(aes(F_score, Dating_flag, fill = Dating_flag)) +
  geom_violin(show.legend = F, alpha = 0.7) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "F score",
       y = "dating flag",
       title = "Dating Flag and F Score")

p22 <- richmondway |>
  ggplot(aes(F_score, Coaching_flag, fill = Coaching_flag)) +
  geom_violin(show.legend = F, alpha = 0.7) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "F score",
       y = "coaching flag",
       title = "Coaching Flag and F Score")

p21 + p22

#ggsave("plot2.png", width = 8)

# Plot 3

richmondway |>
  ggplot(aes(Season, factor(Episode), fill = Imdb_rating)) +
  geom_tile() +
  scale_fill_gradient(low = "lightgreen",
                      high = "darkgreen") +
  labs(x = "season",
       y = "episode",
       fill = "imdb rating",
       title = "IMDB Rating For All Episodes") 

#ggsave("plot3.png", height = 6, width = 8)











 