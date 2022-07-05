library(tidyverse)
library(ggDoubleHeat)
theme_set(theme_bw())

rent <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')
permits <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/sf_permits.csv') %>%
  mutate(location = str_remove_all(location, "POINT \\(|\\)")) %>%
  separate(location, into = c("long", "lat"), sep = " ", convert = T)
new_construction <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/new_construction.csv')

# Plot 1

new_construction %>%
  pivot_longer(7:9, names_to = "production") %>%
  mutate(production = fct_recode(production, 
                                 `Single Family` = "sfproduction",
                                 `Multi Family` = "mfproduction",
                                 `Mobile Home` = "mhproduction"),
         county = fct_reorder(county, -value, sum)) %>%
  ggplot(aes(year, value, fill = production)) +
  geom_area(alpha = 0.6) +
  facet_wrap(~county) +
  labs(x = "",
       y = "production",
       fill = "production type",
       title = "Yearly New Construction Per County")

#ggsave("plot1.png", height = 8, width = 10)

# Plot 2

permits %>%
  ggplot(aes(long, lat, color = permit_type_definition)) +
  geom_point(alpha = 0.1, size = 0.5) +
  theme_void() +
  guides(colour = guide_legend(override.aes = list(alpha = 1,
                                                   size = 1))) +
  labs(color = "permit type",
       title = "Permit Map and its Types") +
  theme(legend.position = "bottom")

#ggsave("plot2.png", height = 8, width = 10, bg = "white")

# Plot 3

rent %>%
  filter(!is.na(county)) %>%
  mutate(county = str_to_title(county)) %>%
  group_by(year, county) %>%
  summarize(mean_price = mean(price, na.rm = T),
            median_price = median(price, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(year, county)) +
  geom_heat_grid(outside = mean_price,
                 outside_name = "mean price",
                 inside = median_price,
                 inside_name = "median price",
                 labels = scales::dollar) +
  ggtitle("Yearly Mean and Median Rent Price Per County")

#ggsave("plot3.png", height = 8, width = 10)


# Plot 4

rent %>%
  mutate(city = fct_lump(city, n = 20)) %>%
  filter(year > 2000) %>%
  ggplot(aes(price, city, fill = city, color = city)) +
  geom_boxplot(show.legend = F, alpha = 0.5) +
  scale_x_log10(labels = scales::dollar,
                n.breaks = 4) +
  facet_wrap(~year, ncol = 6) +
  theme(strip.text = element_text(size = 12),
        plot.title = element_text(size = 15)) +
  labs(x = "",
       title = "Yearly Rent Overview Per City")

#ggsave("plot4.png", height = 9, width = 10)


  


























  
