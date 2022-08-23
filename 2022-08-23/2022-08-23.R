library(tidyverse)
theme_set(theme_light())

chips <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-23/chips.csv')

# Plot 1

chips_long <- 
  chips %>%
  pivot_longer(2:5, names_to = "property") %>%
  mutate(property = fct_recode(property, 
                               "process size (nm)" = "process_size_nm",
                               "thermal design profile" = "tdp_w",
                               "die size" = "die_size_mm_2",
                               "transistors (million)" = "transistors_million"
  )) 

chips_long %>%
  ggplot(aes(year, value, fill = property)) +
  geom_area(alpha = 0.7, show.legend = F) +
  facet_wrap(~property, scales = "free") +
  labs(x = NULL,
       y = NULL,
       title = "Chips and Their Properties Per Year") +
  theme(strip.text = element_text(size = 13))

#ggsave("plot1.png")

# Plot 2

chips %>%
  ggplot(aes(process_size_nm, die_size_mm_2)) +
  geom_point(aes(size = transistors_million,
                 color = tdp_w)) +
  geom_text(aes(label = year),
            vjust = 1,
            hjust = 0,
            check_overlap = T) +
  scale_color_viridis_c() +
  labs(x = "process size (nm)",
       y = "die size",
       color = "TDP",
       size = "transistors (million)",
       title = "Process Size VS Die Size")

#ggsave("plot2.png", width = 10)

# Plot 3

chips %>%
  ggplot(aes(year, process_size_nm)) +
  geom_line() +
  geom_point(aes(size = transistors_million, color = die_size_mm_2)) +
  scale_color_gradient(low = "red",
                       high = "green") +
  labs(x = NULL,
       y = "process size (nm)",
       color = "die size",
       size = "transistors (million)",
       title = "How Chips' Process Size Progreesed?")

#ggsave("plot3.png")

# Plot 4

chips_long %>%
  arrange(year) %>%
  group_by(property) %>%
  mutate(lag_value = lag(value),
         velocity = abs(value - lag_value)/lag_value) %>%
  ungroup() %>% 
  na.omit() %>%
  ggplot(aes(year, velocity, fill = property)) +
  geom_col(position = "dodge",
           alpha = 0.7) +
  facet_wrap(~property) +
  theme(legend.position = "none",
        strip.text = element_text(size = 13)) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL,
       y = "progress velocity",
       title = "Chip Progress Velocity Per Property",
       subtitle = "Velocity is computed by the improvement of this year compared to the previous year")

#ggsave("plot4.png")
  








