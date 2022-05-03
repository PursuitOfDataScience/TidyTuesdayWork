library(tidyverse)
library(scales)
library(lubridate)
library(ggDoubleHeat)
theme_set(theme_bw())

capacity <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/capacity.csv')
wind <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/wind.csv')
solar <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/solar.csv')
average_cost <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/average_cost.csv')

# Plot 1

average_cost %>%
  pivot_longer(2:4, names_to = "source", values_to = "cost") %>%
  mutate(source = str_to_title(str_remove(source, "_mwh$")),
         source = fct_reorder(source, -cost, first)) %>%
  ggplot(aes(year, cost, color = source)) +
  geom_line(size = 1) +
  geom_point() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(labels = dollar, n.breaks = 6) +
  scale_x_continuous(n.breaks = 6) +
  labs(x = NULL,
       y = "average cost (MWh)",
       color = NULL,
       title = "Average Gas, Wind, and Solar Cost per Year")

# ggsave("plot1.png", width = 7, height = 5)

# Plot 2

solar %>%
  rename(mwh = solar_mwh,
         capacity = solar_capacity) %>%
  mutate(source = "solar") %>%
  bind_rows(
    wind %>%
      rename(mwh = wind_mwh,
             capacity = wind_capacity) %>%
      mutate(source = "wind")
  ) %>%
  mutate(floor_date = floor_date(date, "quarter")) %>%
  group_by(floor_date, source) %>%
  summarize(across(c(mwh, capacity), mean)) %>%
  ungroup() %>%
  ggplot(aes(floor_date, source)) +
  geom_heat_grid(outside = mwh, 
                 inside = capacity) +
  theme_heat() +
  labs(x = NULL,
       y = NULL,
       title = "Average Quarter Wind & Solar Capacity & MWh") 

#ggsave("plot2.png", width = 7, height = 5)

# Plot 3

capacity %>%
  filter(type != "Other") %>%
  pivot_longer(3:6, names_to = "source", values_to = "gw") %>%
  mutate(pct_source = gw/total_gw,
         source = str_replace(source, "_", " ")) %>%
  ggplot(aes(year, pct_source, fill = source)) +
  geom_col() +
  facet_wrap(~type) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL,
       y = "source percentage",
       title = "How does each source contribute to energy type?") +
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold"))

#ggsave("plot3.png", width = 7, height = 5)

# Plot 4

capacity %>%
  filter(type != "Other") %>%
  select(type, year, total_gw) %>%
  mutate(type = fct_reorder(type, -total_gw, sum)) %>%
  ggplot(aes(year, total_gw, fill = type)) +
  geom_area(alpha = 0.8) +
  facet_wrap(~type, ncol = 2) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        strip.text = element_text(face = "bold")) +
  labs(x = NULL,
       y = "total gigawatts",
       title = "Total Gigawatts from All Types") 

#ggsave("plot4.png", width = 7, height = 5)  








