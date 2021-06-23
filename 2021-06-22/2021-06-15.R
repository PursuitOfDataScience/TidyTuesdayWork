library(tidyverse)
parks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-22/parks.csv')


parks$spend_per_resident_data <-as.numeric(gsub("\\$", "", parks$spend_per_resident_data))


parks %>% group_by(city) %>%
  summarize(`average total points` = mean(total_points, na.rm = TRUE), `average total spending` = mean(spend_per_resident_data, na.rm = TRUE)) %>%
  #arrange(desc(`average total points`)) %>%
  #slice_max(`average total points`, n = 15) %>%
  ggplot(aes(`average total spending`, `average total points`, color = city)) +
  geom_point(size = 6) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(face = "bold")
  ) +
  ggtitle("City-wise Average Total Spending VS Average Total Points")
#ggsave("City-wise Average Total Spending VS Average Total Points.png", width = 20, height = 10)


parks %>% group_by(city) %>%
  summarize(`average total points` = mean(total_points, na.rm = TRUE), `average total spending` = mean(spend_per_resident_data, na.rm = TRUE)) %>%
  arrange(desc(`average total points`)) %>%
  slice_max(`average total points`, n = 15) %>%
  ggplot(aes(`average total spending`, `average total points`, color = city)) +
  geom_point(size = 6) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(face = "bold")
  ) +
  guides(col = guide_legend(ncol = 8)) +
  ggtitle("Top 15 Cities Parks With Highest Average Total Points & Average Total Spending")
#ggsave("Top 15 Cities Parks With Highest Average Total Points & Average Total Spending.png", width = 20, height = 10)
