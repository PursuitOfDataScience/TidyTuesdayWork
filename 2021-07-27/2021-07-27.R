library(tidyverse)
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

olympics$medal <- factor(olympics$medal, levels = c("Gold", "Silver", "Bronze", "NA"))
olympics %>%
  group_by(year, sex, season, medal) %>%
  summarize(count = n()) %>%
  ggplot(aes(year, count, color = sex)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_grid(medal~season, scales = "free_y") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 15),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 15),
    legend.position = "bottom",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 18)
  ) +
  labs(y = "Medal Counts", title = "Medal Counts of Summer&Winter Olymptics for Men&Women")


olympics <- olympics %>%
  mutate(sex = case_when(
    sex == "F"~"Female",
    sex == "M"~"Male"
  ))

olympics %>%
  group_by(medal, team, sex) %>%
  summarize(count = sum(n())) %>%
  arrange(desc(count)) %>%
  filter(!medal == "NA") %>%
  head(150) %>%
  ggplot(aes(team, count, fill = medal)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip() +
  facet_wrap(~sex, scale = "free_x", labeller = label_value) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 15),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 15),
    legend.position = "bottom",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 18)
  ) 
  
