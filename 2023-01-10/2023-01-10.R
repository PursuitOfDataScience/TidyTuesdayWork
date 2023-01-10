library(tidyverse)
theme_set(theme_light())

feederwatch <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv') |>
  filter(longitude < 0)
site_data <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_count_site_data_public_2021.csv')

# Plot 1

feederwatch |>
  ggplot(aes(longitude, latitude, color = snow_dep_atleast)) +
  geom_point(alpha = 0.5) +
  theme_void() +
  coord_map() +
  borders("state") +
  labs(color = "snow depth (minimum)",
       title = "Bird Location and Snow Depth")


#ggsave("plot1.png", bg = "white")


# Plot 2

feederwatch |>
  mutate(subnational1_code = fct_lump(subnational1_code, n = 10)) |>
  ggplot(aes(how_many, subnational1_code, fill = subnational1_code)) +
  geom_violin(show.legend = F) +
  scale_x_log10() +
  labs(x = "# of birds",
       y = NULL,
       title = "# of Birds Per State/Province")

#ggsave("plot2.png")





