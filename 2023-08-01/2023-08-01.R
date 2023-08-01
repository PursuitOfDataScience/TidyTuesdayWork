library(tidyverse)
library(geofacet)
theme_set(theme_light())

states <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-01/states.csv') |>
  select(-contains("km"))

state_name_etymology <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-01/state_name_etymology.csv') 

# Plot 1
states |>
  mutate(water = water_area_mi2/total_area_mi2,
         land = land_area_mi2/total_area_mi2) |>
  pivot_longer(cols = c(water, land), values_to = "pct") |>
  ggplot(aes(pct, name, fill = name)) +
  geom_col(show.legend = F) +
  facet_geo(~state) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  labs(x = NULL,
       y = NULL,
       title = "Water and Land Area Percentage Per State")

#ggsave("plot1.png", width = 8, height = 6)

# Plot 2
states |>
  ggplot(aes(admission, population_2020)) +
  geom_point(aes(color = n_representatives)) +
  geom_text(aes(label = postal_abbreviation), 
            check_overlap = T, 
            size = 2,
            vjust = -0.5,
            hjust = 0.5) +
  scale_color_gradient(low = "lightgreen", high = "darkgreen") + 
  theme(legend.position = "bottom",
        panel.grid = element_blank()) +
  labs(y = "population in 2020",
       x = "admission to US",
       color = "# of representatives",
       title = "State Admission Date, Population, and # of Representatives")

#ggsave("plot2.png", width = 6)

# Plot 3
state_name_etymology |>
  ggplot(aes(date_named, state, group = state, color = state)) +
  geom_point() +
  geom_text(aes(label = language),
            check_overlap = T,
            size = 2,
            vjust = -1) +
  theme(legend.position = "none") +
  labs(x = "name birthday",
       y = "",
       title = "States and Birthday of Their Names")

#ggsave("plot3.png", height = 6, width = 6)





