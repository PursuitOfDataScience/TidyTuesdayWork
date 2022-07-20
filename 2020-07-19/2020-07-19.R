library(tidyverse)
library(countrycode)

theme_set(theme_bw())

technology <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')

data_for_map <- technology %>%
  filter(str_detect(label, "immunization")) %>%
  filter(year == 2019) %>%
  mutate(label = str_extract(label, "a .+ immunization"),
         label = str_remove_all(label, "a | immunization")) %>%
  rename(immunization = label) %>%
  right_join(
    map_data("world") %>%
      tibble() %>%
      mutate(iso3c = countrycode(region,
                                 destination = "iso3c",
                                 origin = "country.name")),
    by = "iso3c"
) 

# Plot 1
data_for_map %>%
  filter(immunization != "YFV") %>%
  ggplot(aes(long, lat, group = group.y, fill = value)) +
  geom_polygon() +
  scale_fill_gradient(low = "red",
                      high = "green",
                      labels = scales::percent_format(scale = 1)) +
  facet_wrap(~immunization) +
  ggthemes::theme_map() +
  theme(strip.text = element_text(size = 13),
        plot.title = element_text(size = 17)) +
  labs(title = "% of Children Vaccine Immunization in 2019",
       fill = "% children immunization") 


#ggsave("plot1.png", height = 8, width = 10, bg = "white")

# Plot 2

technology %>%
  filter(category == "Financial",
         !str_detect(label, "USD")) %>%
  group_by(label, year) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(label = fct_reorder(label, -value, sum)) %>%
  ggplot(aes(year, value, color = label)) +
  geom_line(size = 1) +
  geom_point() +
  scale_y_log10() +
  theme(legend.position = "bottom") +
  labs(x = NULL,
       y = NULL,
       color = NULL,
       title = "Worldwide Financial Values from 1987 to 2020")

#ggsave("plot2.png", height = 8, width = 10)

# Plot 3

technology %>%
  filter(category == "Energy",
         str_detect(label, "Electric")) %>%
  mutate(electric_source = str_remove_all(label, "Electricity from | \\(TWH\\)")) %>%
  filter(!str_detect(electric_source, "Electric")) %>%
  select(-label) %>%
  group_by(year, electric_source) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(electric_source = str_to_title(electric_source)) %>%
  ggplot(aes(year, electric_source, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "green",
                       low = "red",
                       mid = "pink",
                       midpoint = 4000) +
  labs(x = NULL,
       y = "",
       fill = "TWH",
       title = "Worldwide Electric Generated Sources") 

#ggsave("plot3.png", height = 8, width = 10)

# Plot 3

technology %>%
  filter(category == "Agriculture",
         label == "% Arable land share in agricultural land",
         iso3c %in% c("CHN", "USA", "JPN", "THA", "BRA", "GBR")) %>%
  mutate(iso3c = fct_reorder(iso3c, value, sum)) %>%
  ggplot(aes(year, value, fill = iso3c)) +
  geom_area(alpha = 0.7) + 
  facet_wrap(~iso3c) +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2010)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = NULL,
       y = "% arable land share in agricultural land",
       title = "% Arable Land Share in Agricultural Land across 6 Countries") 

#ggsave("plot4.png", height = 8, width = 10)













