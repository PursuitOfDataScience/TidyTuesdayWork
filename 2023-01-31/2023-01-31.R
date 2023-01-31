library(tidyverse)
library(lubridate)
theme_set(theme_light())

cats_uk <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk.csv')
cats_uk_reference <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk_reference.csv')


# Plot 1

cats_uk |>
  ggplot(aes(location_long, location_lat, size = ground_speed, color = height_above_ellipsoid)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c() +
  labs(x = "long",
       y = "lat",
       size = "ground speed",
       color = "height",
       title = "Locations, Heights, and Speeds")

#ggsave("Plot1.png")


# Plot 2

cats_uk |>
  filter(height_above_ellipsoid > 0,
         ground_speed > 0) |>
  mutate(month = month(timestamp, label = T)) |>
  ggplot(aes(height_above_ellipsoid, ground_speed, color = month)) +
  geom_point(alpha = 0.2) +
  scale_x_log10() +
  scale_y_log10() + 
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  labs(x = "height above ellipsoid",
       y = "ground speed",
       color = NULL,
       title = "Height and Ground Speed Per Month")

#ggsave("Plot2.png")

# Plot 3

cats_uk_reference |>
  mutate(month = month(deploy_on_date, label = T),
         animal_sex = ifelse(animal_sex == "m", "Male", "Female")) |>
  ggplot(aes(hrs_indoors, animal_sex, fill = month)) +
  geom_boxplot(alpha = 0.6) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "hours indoors",
       y = "",
       fill = "",
       title = "# of Hours Indoors Per Month")


#ggsave("Plot3.png")

# Plot 4

cats_uk_reference |>
  filter(age_years > 0) |>
  mutate(animal_sex = ifelse(animal_sex == "m", "Male", "Female")) |>
  ggplot(aes(age_years, prey_p_month, color = animal_sex)) +
  geom_jitter() +
  labs(x = "age",
       y = "prey per month",
       color = NULL,
       title = "Age and # of Monthly Prey") +
  scale_color_brewer(palette = "Set1") 

#ggsave("Plot4.png")


