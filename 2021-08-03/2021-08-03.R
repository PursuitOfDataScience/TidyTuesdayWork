library(tidyverse)
athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv')

athletes$medal <- factor(athletes$medal, levels = c("Gold", "Silver", "Bronze"))

#plot 1
athletes %>% 
  #filter(gender %in% c("Men", "Women")) %>%
  group_by(year, medal, gender) %>%
  summarize(`count` = n()) %>%
  ggplot(aes(year, count, color = gender)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.5) +
  facet_wrap(~medal, scales = "free") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 16),
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
  ) +
  labs(y = "Medal Counts", title = "Time Series Total Counts Of Medals")
#ggsave("Time Series Total Counts Of Medals.png", width = 20, height = 10)
#plot 2
athletes %>% 
  filter(gender %in% c("Men", "Women")) %>%
  group_by(year, medal, gender, abb) %>%
  summarize(`count` = n()) %>%
  arrange(desc(count)) %>%
  filter(count > 20) %>%
  ggplot(aes(year, count, fill = abb)) +
  geom_bar(stat = "identity") +
  facet_grid(medal~gender, scales = "free") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 16),
    legend.title = element_blank(),
    legend.text = element_text(size = 8)
  )+
  labs(y = "Medal Counts", title = "Country-Wise Time Series Total Counts Of Medals (>20)")
#ggsave("Country-Wise Time Series Total Counts Of Medals.png", width = 20, height = 10)  
  
  
  