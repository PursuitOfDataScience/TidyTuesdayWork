library(tidyverse)
theme_set(theme_bw())

blackpast <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/blackpast.csv')
census <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/census.csv')
slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')
african_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')
firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')


# Plot 1

census %>%
  filter(region != "USA Total",
         is.na(division)) %>% 
  pivot_longer(cols = 5:8,
               values_to = "pop") %>%
  mutate(name = str_replace(name, "_", " "),
         name = fct_reorder(name, -pop)) %>%
  ggplot(aes(year, pop, color = name)) +
  geom_line(size = 1) +
  facet_wrap(~region) +
  theme(panel.grid = element_blank()) +
  labs(x = NULL,
       y = "population",
       color = NULL,
       title = "Population Across the Four Regions")

#ggsave("plot1.png", height = 8, width = 10)

# Plot 2

african_names %>%
  mutate(arrival_decade = 10 * year_arrival %/% 10) %>%
  filter(arrival_decade != 1850) %>%
  ggplot(aes(age, height, color = arrival_decade)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "loess", se = F, color = "midnightblue") +
  scale_color_gradient(high = "red",
                       low = "green") +
  labs(color = "arrival decade",
       title = "Age and Height per Decade") +
  facet_wrap(~arrival_decade) +
  theme(legend.position = "none")

#ggsave("plot2.png", height = 8, width = 10)

# Plot 3

slave_routes %>%
  filter(fct_lump(port_arrival, n = 20) != "Other") %>%
  group_by(port_arrival) %>%
  summarize(min_year = min(year_arrival),
            max_year = max(year_arrival),
            avg_slaves = mean(n_slaves_arrived, na.rm = T)) %>%
  ungroup() %>%
  mutate(port_arrival = fct_reorder(port_arrival, -(max_year - min_year))) %>%
  ggplot(aes(y = port_arrival, color = port_arrival)) +
  geom_point(aes(size = avg_slaves, x = (min_year + max_year)/2)) +
  geom_errorbarh(aes(xmin = min_year,
                     xmax = max_year),
                 height = 0.2) +
  scale_color_discrete(guide = "none") +
  labs(x = NULL,
       y = "port arrival",
       size = "average # of slaves",
       title = "Top 20 Ports' Earliest and Final Slave Transport")

#ggsave("plot3.png", height = 8, width = 10)

# Plot 4

firsts %>%
  mutate(decade = 10 * year %/% 10) %>%
  count(decade, category, sort = T) %>%
  mutate(category = fct_reorder(category, -n, sum)) %>%
  ggplot(aes(decade, n, fill = category)) +
  geom_area() +
  facet_wrap(~category) +
  theme(legend.position = "none") +
  labs(y = "# of firsts",
       title = "# of Firsts per Decade per Category")

#ggsave("plot4.png", height = 8, width = 10)






