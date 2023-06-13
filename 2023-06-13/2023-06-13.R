library(tidyverse)
theme_set(theme_bw())

safi_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-13/safi_data.csv')

# Plot 1
safi_data |>
  ggplot(aes(years_liv, no_membrs, color = village)) +
  geom_point() +
  labs(x = "number of years",
       y = "number of members",
       title = "Number of Years Lived in Village and Number of Members")

#ggsave("plot1.png", width = 6) 

# Plot 2
safi_data |>
  mutate(respondent_wall_type = fct_reorder(respondent_wall_type, rooms)) |>
  ggplot(aes(rooms, respondent_wall_type, fill = village, color = village)) +
  geom_violin(alpha = 0.5) +
  scale_x_continuous(breaks = seq(1,8,2)) +
  labs(x = "# of rooms",
       y = "wall type",
       title = "Respondent Wall Type and # of Household Rooms")

#ggsave("plot2.png", width = 6)

# Plot 3
safi_data |>
  separate_rows(items_owned) |> 
  count(village, items_owned, sort = T) |>
  filter(items_owned != "NULL") |>
  complete(village, items_owned, fill = list(n = 0)) |>
  ggplot(aes(village, items_owned, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "lightgreen",
                      high = "darkgreen") +
  labs(y = "item owned",
       fill = "count",
       title = "Villages and Items Owned")

#ggsave("plot3.png")

# Plot 4
safi_data |>
  separate_rows(months_lack_food) |>
  count(village, months_lack_food, sort = T) |>
  complete(village, months_lack_food, fill = list(n = 0)) |>
  mutate(months_lack_food = factor(months_lack_food, levels = month.abb)) |>
  filter(months_lack_food != "NA") |>
  ggplot(aes(village, months_lack_food, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "lightgreen",
                      high = "darkgreen") +
  labs(y = "",
       fill = "count",
       title = "Months Lack of Food per Village") 

#ggsave("plot4.png")






