library(tidyverse)
library(lubridate)
holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')
holidays$weekday <- factor(holidays$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
holidays$month <- factor(holidays$month, levels =  month.abb)
holidays$decade <-  round(holidays$year_of_event / 10) * 10


holidays %>% group_by(month, weekday) %>%
  summarize(count = n()) %>%
  drop_na() %>%
  ggplot(aes(month, count, fill = month)) +
  geom_bar(stat = "identity") +
  facet_wrap(~weekday, scales = "free") +
  coord_flip() +
  theme_bw() +
  theme(
    strip.text = element_text(size = 15),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 17),
    plot.title = element_text(size = 19)
  ) +
  ggtitle("Weekday-Wise Monthly Counts of Independent Day Of The Year")

#ggsave("Weekday-Wise Monthly Counts of Independent Day Of The Year.png", width = 20, height = 10)


holidays %>% group_by(month, weekday) %>%
  summarize(count = n()) %>%
  drop_na() %>%
  ggplot(aes(weekday, count, fill = weekday)) +
  geom_bar(stat = "identity") +
  facet_wrap(~month, scales = "free") +
  coord_flip() +
  theme_bw() +
  theme(
    strip.text = element_text(size = 15),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    axis.text = element_text(size = 15),
    axis.title.x = element_text(size = 17),
    plot.title = element_text(size = 19)
  ) +
  ggtitle("Monthly-Wise Weekday Counts of Independent Day Of The Year")

#ggsave("Monthly-Wise Weekday Counts of Independent Day Of The Year.png", width = 20, height = 10)

decades <- holidays %>% group_by(independence_from, decade) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  drop_na() 

decades %>% filter(decade >= 1900, count > 1)  %>%
  ggplot(aes(independence_from, count, fill = independence_from)) +
  geom_bar(stat = "identity") +
  facet_wrap(~decade) +
  coord_flip() +
  theme_bw() +
  theme(
    strip.text = element_text(size = 15),
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10, angle = 25),
    plot.title = element_text(size = 16)
  ) +
  ggtitle("Decade-Wise Counts On Countries Allowing Independence (count > 1)")
 
#ggsave("Decade-Wise Counts On Countries Allowing Independence.png", width = 20, height = 10) 
  
  
