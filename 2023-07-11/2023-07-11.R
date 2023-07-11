library(tidyverse)
theme_set(theme_light())

global_temps <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/global_temps.csv')
nh_temps <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/nh_temps.csv')
sh_temps <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/sh_temps.csv')
zonann_temps <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/zonann_temps.csv')

line_func <- function(tbl) {
  tbl |>
    pivot_longer(cols = 2:13, names_to = "month") |>
    mutate(month = fct_inorder(month)) |>
    ggplot(aes(Year, value, color = month)) +
    geom_line(show.legend = F) +
    facet_wrap(~month)
}

global_temps |>
  line_func() +
  labs(x = "year",
       y = "temp",
       title = "Average Global Monthly Temp from 1880 to 2023")

#ggsave("plot1.png", width = 6)

nh_temps |>
  line_func() +
  scale_color_viridis_d() +
  labs(x = "year",
       y = "temp",
       title = "Average Northern Hemispheric Monthly Temp from 1880 to 2023")

#ggsave("plot2.png", width = 6)

sh_temps |>
  line_func() +
  scale_color_brewer(palette = "Set3") +
  labs(x = "year",
       y = "temp",
       title = "Average Southern Hemispheric Monthly Temp from 1880 to 2023")

#ggsave("plot3.png", width = 6)

zonann_temps |>
  line_func() +
  labs(x = "year",
       y = "",
       title = "Average Zone Annual Monthly Temp from 1880 to 2023")

#ggsave("plot4.png", width = 6)
