library(tidyverse)
library(ggDoubleHeat)
theme_set(theme_bw())

flights <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv') %>%
  janitor::clean_names() %>%
  mutate(flt_date = lubridate::ymd(str_remove(flt_date, "\\s.+$")))

# Plot 1

flights %>%
  group_by(year, month_mon, month_num) %>%
  summarize(avg_dep = mean(flt_dep_1, na.rm = T),
            avg_arr = mean(flt_arr_1, na.rm = T)) %>%
  ungroup() %>%
  mutate(month_mon = fct_reorder(month_mon, as.numeric(month_num))) %>%
  ggplot(aes(year, month_mon)) +
  geom_heat_grid(inside = avg_dep,
                 inside_name = "average departure",
                 outside = avg_arr,
                 outside_name = "average arrival") +
  theme_heat() +
  scale_x_continuous(breaks = seq(2016, 2022)) +
  labs(title = "Average # of Departure/Arrival Per Year Per Month",
       x = NULL)

#ggsave("plot1.png", width = 11, height = 7)

# Plot 2

flights %>%
  filter(flt_dep_1 > 0,
         flt_arr_1 > 0) %>%
  # sample_n(1000) %>%
  mutate(month_mon = fct_reorder(month_mon, as.numeric(month_num))) %>%
  ggplot(aes(flt_dep_1, flt_arr_1, color = month_mon)) +
  geom_point(alpha = 0.1, size = 0.5, show.legend = F) +
  #geom_text(aes(label = state_name), check_overlap = T, size = 3) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~month_mon) +
  labs(x = "flight departure #",
       y = "flight arrival #",
       title = "# of Departure & Arrival per Month")

#ggsave("plot2.png", width = 10, height = 8)

# Plot 3

flights %>%
  mutate(state_name = fct_lump(state_name, 10, w = flt_tot_1),
         state_name = fct_reorder(state_name, flt_tot_1)) %>%
  ggplot(aes(flt_tot_1, state_name, fill = state_name)) +
  geom_violin(show.legend = F) +
  scale_x_log10() +
  labs(x = "total # of flights",
       y = NULL,
       title = "Total # of Flights for 10 Flight Busiest Countries")

#ggsave("plot3.png", width = 10, height = 8)

# Plot 4

flights %>%
  mutate(apt_name = fct_lump(apt_name, n = 11, w = flt_tot_1),
         year_month = lubridate::make_date(year, month_num)) %>%
  group_by(year_month, apt_name) %>%
  summarize(avg_flt_tot = mean(flt_tot_1)) %>%
  ungroup() %>%
  ggplot(aes(year_month, avg_flt_tot, fill = apt_name)) +
  geom_area(alpha = 0.7, show.legend = F) +
  facet_wrap(~apt_name) +
  labs(x = "",
       y = "average # of daily flights",
       title = "The Busiest Airports in Europe") 
  
#ggsave("plot4.png", width = 10, height = 8)








