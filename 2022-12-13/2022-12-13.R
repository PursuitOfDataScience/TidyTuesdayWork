library(tidyverse)
library(lubridate)
library(geofacet)
library(scales)
theme_set(theme_light())

state_retail <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-13/state_retail.csv',  col_types = "cciciiccc") |>
  mutate(date = make_date(year, month),
         across(c(change_yoy, change_yoy_se), as.numeric)) |> 
  select(-c(year, month)) 

# Plot 1

state_retail |>
  filter(state_abbr == "USA") |> 
  ggplot(aes(x = date, fill = subsector, color = subsector)) +
  geom_line(aes(y = change_yoy)) +
  geom_ribbon(aes(ymin = change_yoy - change_yoy_se,
                  ymax = change_yoy + change_yoy_se),
              alpha = 0.5) +
  facet_wrap(~subsector, scales = "free_y") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme(legend.position = "none") +
  labs(x = "",
       y = "change % year over year",
       title = "Respective Subsector Change Year Over Year in the U.S.")

#ggsave("plot1.png", width = 10, height = 6)


# Plot 2

state_retail |>
  filter(state_abbr != "USA",
         subsector == "total") |> 
  ggplot(aes(x = date, fill = state_abbr, color = state_abbr)) +
  geom_line(aes(y = change_yoy)) +
  geom_ribbon(aes(ymin = change_yoy - change_yoy_se,
                  ymax = change_yoy + change_yoy_se),
              alpha = 0.5) +
  facet_geo(~state_abbr, scales = "free_y") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme(legend.position = "none") +
  labs(x = "",
       y = "change % year over year",
       title = "State-Wise Total Respective Subsector Change Year Over Year")

#ggsave("plot2.png", width = 20, height = 8)


# Plot 3

state_retail |>
  filter(state_abbr != "USA",
         change_yoy <= 1000) |>
  mutate(date = year(date)) |>
  ggplot(aes(date, change_yoy, color = subsector, fill = subsector, group = date)) +
  geom_violin(alpha = 0.6) +
  scale_y_log10(labels = percent_format(scale = 1)) +
  facet_wrap(~subsector) +
  theme(legend.position = "none") 

#ggsave("plot3.png", width = 10, height = 6)











