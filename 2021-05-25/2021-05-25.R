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

#ggsave("trackbarplot.png", width = 20, height = 10)

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

#ggsave("weekdaylineplot.png", width = 20, height = 10)

#another day of plot  --------------------------------------------------
drivers %>% select(position, player, nation) %>%
  distinct() %>%
  group_by(nation) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  #ggplot(aes(nation, count, fill = nation)) +
  ggplot(aes(reorder(nation, -count), count, fill = nation)) +
  
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.title = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
    legend.position = "none"
  )+
  labs(x = "", y = "Player Number")
#ggsave("nationbarplot.png", width = 20, height = 10)

library(ggridges)
drivers %>% group_by(nation) %>%
  summarize(records) %>%
  ggplot(aes(x = records, y = nation, fill = nation)) + 
  geom_density_ridges(scale = 1.5, alpha = 0.7, jittered_points = TRUE) +
  #geom_ridgeline() + 
  #scale_fill_viridis_d(name = "") +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    legend.position = "none",
    title = element_text(size = 15)
  ) + 
  xlim(-5, 50) + 
  ggtitle("Record Density Per Country (1997 - 2020)")

#ggsave("ridgeplot.png", width = 20, height = 10)


#-------------------------------------------------------
library(ggthemes)
unique(records$system_played)
records %>% group_by(system_played, type, year) %>%
  summarize(average_time = mean(time)) %>%
  ggplot(aes(year, average_time, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~system_played) +
  theme_bw() +
  theme_solarized() + 
  labs(x = "", y = "Average Time (seconds)") +
  theme(
    strip.text = element_text(size = 15),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "top",
    legend.text = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = seq(1997, 2020, by = 1)) +
  coord_flip()
ggsave("BarplotNewStyle.png", width = 20, height = 10)

records$month <- as.character(records$month)
records$month <- factor(records$month, levels = c("1", "2", "3", "4", "5", "6", "7", "8",  "9", "10", "11", "12"))
records <- records %>%
  # Rename 4 to 4wd, f to Front, r to Rear
  mutate(month_update = recode(month, "1" = "Jan", "2" = "Feb", "3" = "Mar", "4" = "Apr", "5" = "May", "6" = "Jun", 
                               "7" = "Jul", "8" = "Aug", "9" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec"))

records %>% group_by(year, month_update, type, system_played) %>%
  summarize(average_time = mean(time)) %>%
  ggplot(aes(year, average_time, color = type)) +
  geom_line(size = 2) +
  facet_grid(vars(month_update), vars(system_played), scales = "free") + 
  theme_bw() +
  theme_solarized() + 
  labs(x = "", y = "Average Time (seconds)") +
  theme(
    strip.text = element_text(size = 15),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "top",
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 15)
  ) +
  scale_x_continuous(breaks = seq(1997, 2020, by = 3))
ggsave("LineplotNewStyle.png", width = 20, height = 10)



