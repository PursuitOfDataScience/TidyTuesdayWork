library(tidyverse)
theme_set(theme_light())

owid_energy <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv')

# Plot 1

owid_energy |>
  filter(country == "World") |> 
  select(year, population, coal_prod_per_capita, gas_prod_per_capita, oil_prod_per_capita, solar_energy_per_capita, wind_energy_per_capita) |>
  pivot_longer(2:7) |>
  mutate(name = str_to_title(str_replace_all(name, "_", " "))) |>
  ggplot(aes(year, value, color = name)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y") +
  theme(legend.position = "none") +
  labs(x = NULL,
       y = "value with respective unit",
       title = "World Population, Coal, Gas, and Oil Production from 1900 to 2021")

#ggsave("plot1.png")


# Plot 2

owid_energy |>
  filter(country %in% c("Africa", "Asia", "North America", "South America", "Europe", "Oceania")) |> 
  select(year, country, population, coal_prod_per_capita, gas_prod_per_capita, oil_prod_per_capita, solar_energy_per_capita, wind_energy_per_capita) |>
  pivot_longer(3:8) |>
  mutate(name = str_to_title(str_replace_all(name, "_", " "))) |>
  ggplot(aes(year, value, color = country)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y") +
  theme(legend.position = "bottom") +
  labs(x = NULL,
       y = "value with respective unit",
       color = NULL,
       title = "Continental Population, Coal, Gas, and Oil Production from 1900 to 2021")


#ggsave("plot2.png", bg = "white")

# Plot 3

owid_energy |>
  filter(country %in% c("United States", "China", "Japan", "Singapore", "Germany", "France")) |>
  select(year, country, population, coal_consumption, gas_consumption, oil_consumption, solar_consumption, wind_consumption) |>
  pivot_longer(3:8) |>
  mutate(name = str_to_title(str_replace_all(name, "_", " "))) |>
  ggplot(aes(year, value, color = country)) +
  geom_line() +
  facet_wrap(~name, scales = "free") +
  theme(legend.position = "bottom") +
  labs(x = NULL,
       y = "value with respective unit",
       color = NULL,
       title = "Population and Energy Consumption for the Six Countries")


#ggsave("plot3.png")

# Plot 4

map_data("world") |>
  tibble() |>
  filter(region != "Antarctica") |>
  mutate(region = ifelse(region == "USA", "United States", region)) |>
  left_join(owid_energy |>
               filter(year == 2020), by = c("region" = "country")) |>
  ggplot(aes(long, lat, group = group, fill = electricity_generation)) +
  geom_polygon() +
  scale_fill_gradient(low = "lightgreen",
                      high = "darkgreen",
                      trans = "log") +
  theme_void() +
  theme(legend.position = "right") +
  labs(fill = "electricity generation",
       title = "Electricity Generation in 2020") 

#ggsave("plot4.png", bg = "white")


