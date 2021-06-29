library(tidyverse)
library(lubridate)
animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')

animal_rescues$date_time_of_call <- dmy_hm(animal_rescues$date_time_of_call)
animal_rescues$incident_notional_cost <- as.numeric(animal_rescues$incident_notional_cost)

animal_rescues <- animal_rescues %>% 
  mutate(month = month(animal_rescues$date_time_of_call))

animal_rescues %>% group_by(property_category, month) %>%
  summarize(`average incidence notional cost` = mean(incident_notional_cost, na.rm = TRUE)) %>%
  ggplot(aes(month, `average incidence notional cost`, fill = month)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~property_category) +
  scale_x_continuous(breaks = seq(1, 12)) +
  scale_fill_gradient(low="blue", high="red") + 
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 15),
    axis.ticks = element_blank()
  ) +
  ggtitle("Monthly Average Incidence Cost Faceted By Property Category")


# transforming month into character
animal_rescues <- animal_rescues %>% 
  mutate(month = month(animal_rescues$date_time_of_call,abbr=TRUE,label=TRUE))

animal_rescues %>% group_by(property_category, month) %>%
  summarize(`average incidence notional cost` = mean(incident_notional_cost, na.rm = TRUE)) %>%
  ggplot(aes(property_category, `average incidence notional cost`, fill = property_category)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~month) +
  #scale_fill_gradient(low="blue", high="red") + 
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 13),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 15)
  ) +
  coord_flip() +
  ggtitle("Monthly Property Category-Wise Incidence Cost")
