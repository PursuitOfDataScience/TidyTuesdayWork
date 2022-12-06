library(tidyverse)
library(lubridate)
theme_set(theme_light())

elevators <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-06/elevators.csv') |>
  janitor::clean_names() |>
  filter(longitude > -75) |>
  mutate(dv_lastper_insp_date = ymd(dv_lastper_insp_date),
         dv_approval_date = ymd(dv_approval_date))


# Plot 1

elevators |>
  ggplot(aes(longitude, latitude, color = dv_device_status_description)) +
  geom_point(alpha = 0.5) +
  labs(color = NULL,
       title = "Elevator Location and Status Description in New York")

#ggsave("plot1.png")

# Plot 2

elevators |>
  count(device_type, borough) |>
  mutate(device_type = str_remove(device_type, "\\s.+$")) |>
  na.omit() |>
  complete(device_type, borough, fill = list(n = 0)) |>
  ggplot(aes(borough, device_type, fill = n)) +
  geom_tile() +
  scale_fill_gradient2(high = "green",
                       low = "red",
                       mid = "pink",
                       midpoint = 8000) +
  labs(x = NULL,
       y = "device type",
       fill = "count",
       title = "Elevator Types in All Boroughs")

#ggsave("plot2.png")

# Plot 3

elevators |>
  group_by(dv_device_status_description, borough) |>
  summarize(approval_date = min(dv_approval_date, na.rm = T),
            insp_date = max(dv_lastper_insp_date, na.rm = T)) |>
  ungroup() |>
  na.omit() |>
  ggplot(aes(y = dv_device_status_description, color = dv_device_status_description)) +
  geom_errorbarh(aes(xmin = approval_date,
                     xmax = insp_date),
                 height = 0.5) +
  facet_wrap(~borough) +
  theme(legend.position = "none") +
  labs(y = "status description",
       title = "Earliest Elevator Approval Date and Latest Insepction Date Per Status")

#ggsave("plot3.png", width = 8)

# Plot 4

elevators |>
  count(street_name, device_type, sort = T) |>
  mutate(device_type = str_remove(device_type, "\\s.+$"),
         street_name = fct_lump(street_name, n = 10, w = n)) |>
  filter(street_name != "Other") |>
  mutate(street_name = fct_reorder(street_name, n, sum)) |>
  ggplot(aes(n, street_name, fill = device_type)) +
  geom_col() +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "count",
       y = "",
       fill = "device type",
       title = "Top 10 Streets with Most Number of Elevators")

#ggsave("plot4.png")




