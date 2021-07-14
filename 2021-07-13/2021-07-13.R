library(tidyverse)
library(lubridate)
sco <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')
sco$imdb <- as.numeric(sco$imdb)

# plot1
sco %>% group_by(network) %>%
  summarize(`show counts` = n(), `imdb average` = mean(imdb, na.rm = TRUE), `imdb standard deviation` = sd(imdb, na.rm = TRUE)) %>%
  pivot_longer(!network, names_to = "metric", values_to = "count") %>%
  ggplot(aes(network, count, fill = network)) +
  geom_bar(stat = "identity") +
  facet_grid(~metric, scales = "free") +
  coord_flip() +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    axis.text = element_text(size = 15),
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 18)
  )+
  ggtitle("TV Networks & Related IMDB Stats")
# ggsave("TV Networks & Related IMDB Stats.png", width = 20, height = 10)


#plot2
sco$year <- year(sco$date_aired)

sco %>% group_by(year, network) %>%
  summarize(`imdb average` = mean(imdb, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!network %in% c("Adult Swim", "Syndication", "TBC")) %>%
  ggplot(aes(year, `imdb average`, color = network)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~network, scales = "free_x") +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 18)
  ) +
  ggtitle("Network Time Series Average imdb Score")

#ggsave("Network Time Series Average imdb Score.png", width = 20, height = 10)
