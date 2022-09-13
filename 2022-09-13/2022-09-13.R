library(tidyverse)
library(geofacet)
library(tidytext)
library(tidylo)
theme_set(theme_light())

bigfoot <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bfro_reports_geocoded.csv") |>
  filter(season != "Unknown") |>
  mutate(season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter")))

# Plot 1

bigfoot |>
  group_by(season, state) |> 
  summarize(temp_high = max(temperature_high, na.rm = T),
            temp_mid = mean(temperature_mid, na.rm = T),
            temp_low = min(temperature_low, na.rm = T),
            n = n()) |>
  ungroup() |>
  mutate(across(is.numeric, na_if, Inf),
         across(is.numeric, na_if, -Inf)) |>
  ggplot(aes(y = season, color = season)) +
  geom_point(aes(x = temp_mid)) +
  geom_errorbarh(aes(xmin = temp_low,
                     xmax = temp_high),
                 height = 0.3) +
  facet_geo(~state) +
  theme(legend.position = "none") +
  labs(x = "temperature",
       y = NULL,
       title = "Highest and Lowest Temps Per State Per Season")

#ggsave("plot1.png", height = 6, width = 10)

# Plot 2

bigfoot |>
  select(is.numeric) |>
  select(temperature_mid, dew_point:wind_speed) |> 
  pivot_longer(cols = everything()) |>
  mutate(name = str_to_title(str_replace_all(name, "_"," "))) |>
  ggplot(aes(value, fill = name)) +
  geom_histogram(show.legend = F, alpha = 0.7) +
  facet_wrap(~name, scales = "free")  +
  labs(x = NULL,
       title = "Distribution of the Selected Numeric Variables")

#ggsave("plot2.png")

# Plot 3

bigfoot |>
  unnest_tokens(word, observed) |>
  anti_join(stop_words) |>
  count(word, season) |>
  filter(!str_detect(word, "_|[:digit:]|\\."),
         n > 10) |>
  bind_log_odds(season, word, n) |>
  group_by(season) |>
  slice_max(log_odds_weighted, n = 10, with_ties = F) |>
  ungroup() |>
  mutate(word = reorder_within(word, log_odds_weighted, season)) |>
  ggplot(aes(log_odds_weighted, word, color = season)) +
  geom_point() +
  geom_errorbarh(aes(xmin = 0,
                     xmax = log_odds_weighted),
                 height = 0) +
  scale_y_reordered() +
  facet_wrap(~season, scales = "free_y") +
  theme(legend.position = "none") +
  labs(x = "weighted log odds",
       y = NULL,
       title = "Top 10 Largest Weighted-Log-Odds Observed Words Per Season")

#ggsave("plot3.png")

# Plot 4

bigfoot |>
  filter(longitude > -130) |>
  ggplot(aes(longitude, latitude, color = classification)) +
  geom_point(alpha = 0.5, size = 1) +
  theme_minimal() +
  facet_wrap(~season) +
  labs(title = "Observed Location Points Per Season")

#ggsave("plot4.png", bg = "white")








 




  
