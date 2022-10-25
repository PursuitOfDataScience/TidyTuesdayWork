library(tidyverse)
library(scales)
theme_set(theme_light())

bakers <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/bakers.csv')


# Plot 1

bakers |>
  group_by(series) |>
  summarize(min_age = min(age),
            mean_age = mean(age),
            max_age = max(age),
            .groups = "drop") |>
  ggplot(aes(series, mean_age)) +
  geom_line(aes(color = "average"), size = 2) +
  geom_line(data = bakers |>
              filter(series_winner == 1),
            aes(series, age, color = "winner"), size = 2) +
  geom_ribbon(aes(ymin = min_age,
                  ymax = max_age),
                 alpha = 0.3,
                 fill = "green") +
  expand_limits(y = 0) +
  scale_x_continuous(breaks = seq(1,10)) +
  scale_color_brewer(palette = "Set1") +
  labs(y = "age",
       color = "",
       title = "British Bake Off Contestants's Age Per Series",
       subtitle = "Line means average age, upper and lower bound refer to max and mean age")

#ggsave("plot1.png", width = 7)


# Plot 2

bakers |>
  group_by(series, series_winner) |>
  summarize(median = median(technical_median, na.rm = T),
            minimum = max(technical_lowest, na.rm = T),
            maximum = min(technical_highest, na.rm = T),
            .groups = "drop") |>
  mutate(series_winner = if_else(series_winner == 1, "winner", "loser"),
         series = paste0("Series ", series),
         series = fct_reorder(series, parse_number(series))) |>
  ggplot(aes(median, series_winner, color = series_winner)) +
  geom_point() +
  geom_errorbarh(aes(xmin = maximum,
                     xmax = minimum),
                 height = 0.3) +
  facet_wrap(~series, ncol = 5) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none") +
  labs(x = "series rank",
       y = "",
       title = "Series Ranks between Winners and Losers") 

#ggsave("plot2.png", width = 7)

# Plot 3

bakers |>
  mutate(occupation = str_to_lower(str_remove_all(occupation, "^.+\\s")),
         occupation = fct_lump(occupation, n = 5)) |>
  filter(occupation != "Other") |>
  pivot_longer(4:9) |>
  mutate(name = str_to_title(str_replace(name, "_", " "))) |>
  ggplot(aes(value, fill = occupation)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~name, scales = "free_y") +
  labs(x = NULL,
       fill = NULL,
       title = "Top 5 Occupations and Their Ranking Density")

#ggsave("plot3.png", width = 8)

# Plot 4

bakers |>
  group_by(series) |>
  summarize(q_25 = quantile(total_episodes_appeared, 0.25),
            mean = mean(total_episodes_appeared),
            q_75 = quantile(total_episodes_appeared, 0.75),
            .groups = "drop") |>
  ggplot(aes(series, mean, color = factor(series))) +
  geom_point() +
  geom_errorbar(aes(ymin = q_25,
                    ymax = q_75),
                width = 0.3,
                size = 1) +
  geom_smooth(color = "green", se = F) +
  scale_x_continuous(breaks = seq(1, 10)) +
  theme(legend.position = "none") +
  labs(y = "total episodes appeared",
       title = "Total Epiosdes Appeared Mean, 25th and 75th Quantiles")

#ggsave("plot4.png", width = 6)



