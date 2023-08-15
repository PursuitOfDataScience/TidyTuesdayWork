library(tidyverse)
library(scales)
theme_set(theme_light())

spam <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-15/spam.csv') |>
  rename(`capital characters` = 1,
         spam = 7) 

# Plot 1
spam |>
  pivot_longer(cols = c(1:6), names_to = "feature") |>
  ggplot(aes(value, spam, color = feature, fill = feature)) +
  geom_boxplot(show.legend = F, alpha = 0.5) +
  facet_wrap(~feature, scales = "free_x") +
  scale_x_log10() +
  labs(title = "Email Words and Spam")

#ggsave("plot1.png", width = 6)

# Plot 2
spam |>
  ggplot(aes(dollar, n000, color = spam)) +
  geom_point(alpha = 0.8, size = 1) +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Email Words dollar and 000 Percent")

#ggsave("plot2.png")

# Plot 3
spam |>
  filter(`capital characters` < 3000) |>
  mutate(spam = factor(spam, levels = c("y", "n"))) |>
  ggplot(aes(`capital characters`, fill = spam)) +
  geom_histogram() +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous() +
  labs(title = "Continuous Capital Characters and Spam") 

#ggsave("plot3.png")







