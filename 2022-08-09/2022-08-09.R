library(tidyverse)
theme_set(theme_bw())

wheels <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')

# Plot 1

wheels %>%
  mutate(status = fct_lump(status, 3)) %>%
  filter(!is.na(status)) %>%
  ggplot(aes(diameter, height, color = status)) +
  geom_point(aes(size = seating_capacity), alpha = 0.5) +
  geom_text(aes(label = name),
            hjust = 1,
            vjust = 0,
            check_overlap = T) +
  labs(x = "diameter (feet)",
       y = "height (feet)",
       size = "seating capacity",
       title = "Wheels' Diameter & Height") 

#ggsave("plot1.png", width = 12)


# Plot 2

wheels %>%
  mutate(country = fct_lump(country, 3),
         country = fct_reorder(country, passengers_per_cabin)) %>%
  ggplot(aes(passengers_per_cabin, country, fill = country, color = country)) +
  geom_violin(alpha = 0.5) +
  geom_point(aes(size = ride_duration_minutes)) +
  scale_size_continuous(range = c(1,3)) +
  scale_fill_discrete(guide = "none") +
  scale_color_discrete(guide = "none") +
  labs(x = "passengers/cabin",
       y = NULL,
       size = "ride duration (minutes)",
       title = "Cabin Capacity per Country")

#ggsave("plot2.png")

# Plot 3

wheels %>%
  mutate(year_open = lubridate::year(opened),
         decade_open = 10 * year_open %/% 10,
         country = fct_lump(country, n = 2)) %>%
  group_by(decade_open, country) %>%
  summarize(min_capacity = min(hourly_capacity, na.rm = T),
            mean_capacity = mean(hourly_capacity, na.rm = T),
            max_capacity = max(hourly_capacity, na.rm = T)) %>%
  ungroup() %>% 
  filter(#country %in% c("Japan", "UK", "USA"),
         #decade_open > 1980,
         mean_capacity != Inf) %>% 
  ggplot(aes(x = decade_open, y = mean_capacity, fill = country, color = country)) +
  geom_line() +
  geom_ribbon(aes(ymin = min_capacity,
                  ymax = max_capacity),
              alpha = 0.4) +
  facet_wrap(~country, ncol = 1) +
  scale_y_log10() +
  theme(legend.position = "none") +
  labs(x = "open decade",
       y = "hourly capacity",
       title = "Min, Mean, Max Wheels' Hourly Capacity")

#ggsave("plot3.png")











