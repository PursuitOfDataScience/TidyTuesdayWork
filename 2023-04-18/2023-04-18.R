library(tidyverse)
theme_set(theme_light())

founder_crops <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-18/founder_crops.csv')

founder_crops |>
  count(source_site_name, sort = T)

# Plot 1

founder_crops |>
  filter(!is.na(category)) |>
  ggplot() +
  geom_point(aes(longitude, latitude, color = category), alpha = 0.6) +
  geom_polygon(data = map_data("world") |>
                 tibble() |>
                 filter(lat < 60 & lat > 0,
                        long < 60 & long > 20),
               aes(long, lat, group = group),
               alpha = 0.5,
               fill = "grey") +
  coord_map() +
  scale_color_brewer(palette = "Set1") +
  theme_void() +
  labs(title = "Founder Crop Categories in Southwest Asia")

#ggsave("plot1.png", bg = "white")

# Plot 2

founder_crops |>
  mutate(age_start = -age_start,
         age_end = -age_end) |>
  group_by(family) |>
  summarize(age_start = min(age_start),
            age_end = max(age_end),
            n = n()) |>
  filter(n > 10) |>
  mutate(family = fct_reorder(family, age_start)) |>
  ggplot(aes(y = family)) +
  geom_point(aes(size = n, x = age_start/2 + age_end/2), 
             color = "darkgreen") +
  geom_errorbarh(aes(xmin = age_start,
                     xmax = age_end),
                 height = 0.5) +
  scale_x_continuous(labels = ~ scales::label_number(scale = 1)(abs(.x))) +
  labs(x = "year(BP)",
       y = NULL,
       size = "count",
       title = "Crop Family Age Start and End in Year BP")

#ggsave("plot2.png")

# Plot 3

founder_crops |>
  filter(fct_lump(genus, n = 20) != "Other") |>
  count(genus, category, sort = T) |>
  mutate(genus = fct_reorder(genus, n)) |>
  ggplot(aes(n, genus, fill = category)) +
  geom_col() +
  scale_x_log10() +
  labs(x = "count",
       y = "",
       title = "Genus Counts Per Category")

#ggsave("plot3.png")

# Plot 4

founder_crops |>
  mutate(site_name = fct_lump(site_name, n = 20)) |>
  count(site_name, founder_crop, sort = T) |>
  na.omit() |>
  ggplot(aes(founder_crop, site_name, fill = n)) +
  geom_tile() +
  scale_fill_continuous(trans = "log10",
                        high = "darkgreen",
                        low = "lightgreen") +
  theme(panel.grid = element_blank()) +
  labs(x = "founder crop",
       y = "site",
       fill = "count",
       title = "Founder Crops and Sites")

#ggsave("plot4.png", width = 10)





