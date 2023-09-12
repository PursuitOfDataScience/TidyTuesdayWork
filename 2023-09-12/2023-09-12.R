library(tidyverse)
theme_set(theme_light())

all_countries <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/all_countries.csv') |>
  left_join(country_regions |>
              select(country_name, country_iso3) |>
              distinct(),
            by = "country_iso3")
country_regions <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/country_regions.csv')
global_human_day <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/global_human_day.csv')
global_economic_activity <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/global_economic_activity.csv')

errorbar_plot <- function(tbl) {
  tbl |>
    mutate(Subcategory = fct_reorder(Subcategory, hoursPerDay)) |>
    ggplot(aes(hoursPerDay, Subcategory, color = Subcategory)) +
    geom_point(size = 1) +
    geom_errorbarh(aes(xmin = hoursPerDay - uncertainty,
                       xmax = hoursPerDay + uncertainty),
                   height = 0.6) +
    theme(legend.position = "none") +
    labs(x = "hours per day",
         y = "")
}

# Plot 1
global_economic_activity |>
  errorbar_plot() +
  ggtitle("Global Economic Activity Hours Per Day")

#ggsave("plot1.png", width = 6, height = 4)

# Plot 2
global_human_day |>
  errorbar_plot() + 
  ggtitle("Global Human Activity Hours Per Day")

#ggsave("plot2.png", width = 6, height = 4)

# Plot 3

all_countries |>
  group_by(Category, country_iso3) |>
  summarize(hoursPerDayCombined = sum(hoursPerDayCombined),
            population = max(population)) |>
  ungroup() |>
  ggplot(aes(population, hoursPerDayCombined, color = Category)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_text(aes(label = country_iso3), 
            check_overlap = T, 
            size = 2,
            vjust = -1) +
  scale_x_log10() +
  facet_wrap(~Category) +
  theme(legend.position = "none") +
  labs(y = "hours per day",
       title = "Country Population and Daily Event Hours")

#ggsave("plot3.png", width = 8, height = 6)

# Plot 4
all_countries |>
  mutate(Subcategory = fct_reorder(Subcategory, hoursPerDayCombined)) |>
  ggplot(aes(hoursPerDayCombined, Subcategory, color = Subcategory, fill = Subcategory)) +
  geom_boxplot(alpha = 0.5) +
  theme(legend.position = "none")  +
  labs(x = "hours per day",
       y = "",
       title = "Daily Categorical Hours for All Countries") 

#ggsave("plot4.png", width = 6, height = 4)






