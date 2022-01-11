library(tidyverse)
library(geofacet)
library(scales)
library(tidytext)
theme_set(theme_bw())

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv') %>%
  filter(str_detect(year, "\\d\\d\\d\\d"),
         str_detect(months, "-")) %>%
  mutate(months = factor(months, levels = c("January-March", "April-June", "July-September", "October-December")),
         year = as.numeric(year)) %>%
  left_join(tibble(state_abb = state.abb, state = state.name), by = "state")
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv') %>%
  filter(str_detect(year, "\\d\\d\\d\\d"),
         str_detect(months, "-")) %>%
  mutate(months = factor(months, levels = c("January-March", "April-June", "July-September", "October-December")),
         year = as.numeric(year))

### Join two tibbles together

colony %>%
  right_join(stressor, by = c("year", "months", "state"))


### Plot 1

stressor %>%
  group_by(months, state, stressor) %>%
  summarize(avg_stress_pct = mean(stress_pct, na.rm = T)) %>%
  ungroup() %>%
  mutate(stressor = fct_reorder(stressor, avg_stress_pct, sum)) %>%
  ggplot(aes(avg_stress_pct, stressor, fill = months)) +
  geom_col() +
  facet_geo(~state) +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  labs(x = "stress percentage",
       fill = NULL,
       title = "State-wise average bee stress percentage (2015-2021)") +
  theme(axis.text.x = element_text(angle = 90),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 18),
        axis.title = element_text(size = 14)) 

#ggsave("plot1.png", width = 20, height = 12)


### Plot 2

colony %>%
  ggplot(aes(colony_lost, colony_added)) +
  geom_point(aes(color = months)) +
  geom_smooth(method = "lm", aes(group = 1), se = F) +
  geom_text(aes(label = state_abb, color = months), size = 3, hjust = 1, vjust = 1, check_overlap = T) +
  facet_wrap(~year) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "# of colony lost",
       y = "# of colony added",
       color = NULL,
       title = "The relations between # of bee colony lost and added") +
  theme(legend.position = c(0.6, 0.1),
        strip.text = element_text(size = 13),
        plot.title = element_text(size = 18),
        axis.title = element_text(size = 14))

#ggsave("plot2.png", width = 15, height = 10)

### Plot 3
colony %>%
  ggplot(aes(colony_lost_pct, colony_reno_pct, color = state_abb)) +
  geom_point() +
  geom_text(aes(label = state_abb), size = 3, vjust = -1, hjust = 1, check_overlap = F) +
  facet_grid(year ~ months) +
  theme(legend.position = "none",
        strip.text = element_text(size = 13),
        plot.title = element_text(size = 18),
        axis.title = element_text(size = 14)) +
  scale_x_log10(labels = percent_format(scale = 1)) +
  scale_y_log10(labels = percent_format(scale = 1)) +
  labs(x = "colony lost percentage",
       y = "colony renovated percentage",
       title = "The relations between colony lost % and renovated %")

#ggsave("plot3.png", width = 18, height = 11)

### Plot 4

colony %>%
  filter(state != "United States") %>%
  ggplot(aes(as.factor(year), state, fill = colony_max)) +
  geom_tile() +
  facet_wrap(~months) +
  scale_fill_gradient2(high = "green",
                       low = "red",
                       mid = "pink",
                       midpoint = mean(colony$colony_max, na.rm = T)) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = 13),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 18)) +
  labs(x = NULL,
       y = NULL,
       fill = "max # of colony",
       title = "State-wide yearly maximum # of colony",
       subtitle = "White space means missing value")

ggsave("plot4.png", width = 15, height = 11)






