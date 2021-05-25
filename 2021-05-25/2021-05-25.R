library(tidyverse)
library(lubridate)
records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')

# EDA
head(records)

head(drivers)
sum(is.na(records))
sum(is.na(drivers))
sapply(drivers, function(x) sum(is.na(x))) # 1991 missing records, 125 missing nation

records %>% group_by(shortcut) %>%
  summarize(mean(time))
#it makes sense that taking shortcut would spend less time on average
records$date <- as_date(records$date)

records <- records %>%
  mutate(year = year(records$date), month = month(records$date), day = day(records$date), weekday = weekdays(records$date))

records %>% count(track, sort = T)

records %>% count(year, sort = T)

records %>% group_by(year, track) %>%
  summarize(average_time = mean(time)) %>%
  ggplot(aes(year, average_time, fill = year)) +
  geom_bar(stat = "identity") +
  facet_wrap(~track) +
  theme_bw() +
  theme(
    strip.text = element_text(size=15, face = "bold"),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
    legend.position = "none"
  ) +
  labs(x = "Year", y = "Average Time (seconds)") 

ggsave("trackbarplot.png", width = 20, height = 10)

records$weekday <- factor(records$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
records %>% group_by(year, weekday) %>%
  summarize(average_time = mean(time)) %>%
  ggplot(aes(year, average_time, color = weekday)) +
  geom_line(size = 1.5)+
  facet_wrap(~weekday) +
  theme_bw() +
  theme(
    strip.text = element_text(size=15, face = "bold"),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
    legend.position = "none"
  )+
  labs(x = "Year", y = "Average Time (seconds)")

ggsave("weekdaylineplot.png", width = 20, height = 10)

