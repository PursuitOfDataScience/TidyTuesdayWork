library(tidyverse)
library(geofacet)
library(lubridate)
theme_set(theme_light())

weather_forecasts <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/weather_forecasts.csv')
cities <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/cities.csv')
outlook_meanings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/outlook_meanings.csv')


# simple data cleaning
weather_forecasts <- weather_forecasts |>
  left_join(outlook_meanings, by = "forecast_outlook") |> 
  rename(forecast_outcome = "meaning") |>
  select(-forecast_outlook) |>
  mutate(forecast_hours_before = paste(forecast_hours_before, "Hours Forecast Before"))

# Plot 1

cities |>
  ggplot(aes(lon, lat, color = elevation, size = avg_annual_precip)) +
  geom_point(alpha = 0.5) +
  borders("state") +
  coord_map() +
  theme_void() +
  scale_color_viridis_c() +
  labs(size = "precipitation",
       title = "City Elevation and Annual Average Precipitation")

#ggsave("plot1.png", bg = "white")

# Plot 2

cities |>
  group_by(state) |>
  summarize(wind = mean(wind)) |>
  ungroup() |>
  inner_join(tibble(state = state.abb,
                    region = str_to_lower(state.name))) |>
  inner_join(map_data("state") |>
               tibble(),
             by = "region") |>
  ggplot(aes(long, lat, group = group, fill = wind)) +
  geom_polygon() +
  coord_map() +
  theme_void() +
  scale_fill_viridis_c() +
  labs(fill = "wind speed",
       title = "Average Wind Speed Per State")

#ggsave("plot2.png", bg = "white")

# Plot 3

weather_forecasts |>
  filter(forecast_hours_before == "12 Hours Forecast Before") |>
  mutate(temp_diff = observed_temp - forecast_temp) |>
  group_by(year = year(date), month = month(date), state) |>
  summarize(mean_temp = mean(temp_diff, na.rm = T),
            sd_temp = sd(temp_diff, na.rm = T),
            .groups = "drop") |>
  na.omit() |>
  mutate(date = make_date(year, month)) |>
  ggplot(aes(date, mean_temp, fill = state)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_temp - 1.96 * sd_temp,
                  ymax = mean_temp + 1.96 * sd_temp),
              alpha = 0.5) +
  theme(legend.position = "none") +
  facet_geo(~state) +
  labs(x = NULL,
       y = "temp diff",
       title = "12 Hours Forecast Temp Difference Per State with 95% C.I.")

#ggsave("plot3.png", width = 16, height = 10)

# Plot 4

weather_forecasts |>
  na.omit() |>
  distinct(date, city, state, forecast_outcome) |> 
  mutate(forecast_outcome = fct_lump(forecast_outcome, n = 8)) |>
  count(state, forecast_outcome, sort = T) |>
  ggplot(aes(n, forecast_outcome, fill = forecast_outcome)) +
  geom_col(show.legend = F) +
  facet_geo(~state, scales = "free_x") +
  labs(x = "# of days",
       y = "",
       title = "Weather Forecast Outcome Per State") 

#ggsave("plot4.png", width = 16, height = 12)





