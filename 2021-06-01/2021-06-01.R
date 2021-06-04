library(tidyverse)
summary <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/summary.csv')
challenges <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/challenges.csv')
castaways <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/castaways.csv')
viewers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/viewers.csv')
jury_votes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/jury_votes.csv')

summary %>% group_by(country) %>%
  summarize(premier = mean(viewers_premier), finale = mean(viewers_finale), reunion = mean(viewers_reunion), `Max Rank` = max(rank)) %>%
  gather(viewer_type, viewers, premier: reunion) %>%
  ggplot(aes(country, viewers, fill = `Max Rank`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~viewer_type) +
  coord_flip()+ 
  theme_bw() +
  theme(
    strip.text = element_text(size=15, face = "bold"),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
    legend.position = "top"
  )  + scale_fill_gradient(low="blue", high="red")
  
#ggsave("barplot.png", height = 10, width = 20) 
  
summary %>%
  select(tail(names(.), 5)) %>%
  gather(viewers_type, viewers, viewers_premier: viewers_mean) %>%
  ggplot(aes(x = rank, y = viewers, color = viewers_type)) +
    geom_line(size = 1.2) + 
    geom_point(size = 2) +
  facet_wrap(~viewers_type) +
  theme_bw() +
  theme(
    strip.text = element_text(size=15, face = "bold"),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
    legend.position = "none"
  ) +
  scale_x_continuous(breaks = seq(1,30, by =2))

#ggsave("lineplot.png", height = 10, width = 20)

#-----------------------------------------------------------
library(ggridges)
summary %>%
  separate(timeslot, c("weekday", "time"), " ", extra = "merge") %>%
  group_by(weekday) %>%
  select(tail(names(.), 5)) %>%
  summarize(`viewers premier` = viewers_premier,`viewers finale` = viewers_finale, `viewers reunion` = viewers_reunion, `viewers mean` = viewers_mean) %>%
  gather(`viewers type`, `viewers`, `viewers premier`: `viewers mean`) %>%
  ggplot(aes(x = `viewers`, y = `viewers type`, fill = weekday)) + 
  geom_density_ridges(scale = 1, alpha = 0.7, jittered_points = TRUE) +
  theme_bw() +
  theme(
    strip.text = element_text(size=15, face = "bold"),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
    legend.position = "top",
    legend.text = element_text(size = 15),
    legend.title = element_blank()
  ) 

ggsave("densityplot.png", height = 10, width = 20)