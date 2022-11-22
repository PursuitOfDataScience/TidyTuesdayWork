library(tidyverse)
theme_set(theme_light())

museums <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv') |>
  janitor::clean_names() |>
  filter(latitude < 70)

# plot 1

map_data('world') |>
  tibble() |>
  filter(region == "UK") |>
  ggplot() +
  geom_polygon(aes(long, lat, group = group), fill = "grey") +
  geom_point(data = museums, aes(longitude, latitude, color = size), alpha = 0.6, size = 1.5) +
  coord_map() +
  theme_void() +
  labs(title = "Museum Geolocation & Size")


#ggsave("plot1.png", bg = "white", width = 10, height = 8)


# plot 2

museums |>
  select(where(is.numeric)) |>
  select(-c(1,2,3)) |>
  pivot_longer(everything()) |>
  mutate(name = str_remove(name, "area_"),
         name = str_to_title(str_replace_all(name, "_", " "))) |> 
  ggplot(aes(value, fill = name)) +
  geom_histogram(show.legend = F) +
  facet_wrap(~name, scales = "free", ncol = 4) +
  labs(title = "The Distribution of Respective Museum Indexes")

#ggsave("plot2.png", width = 10, height = 8)

# plot 3

museums |>
  count(domus_subject_matter, accreditation) |>
  na.omit() |>
  mutate(domus_subject_matter = str_to_title(domus_subject_matter)) |>
  complete(domus_subject_matter, accreditation,  fill = list(n = 0)) |> 
  ggplot(aes(accreditation, domus_subject_matter, fill = n)) +
  geom_tile() +
  scale_fill_gradient2(low = "red",
                      high = "green",
                      midpoint = 100,
                      mid = "pink") +
  labs(x = NULL,
       y = NULL,
       fill = "count",
       title = "Museum Subject Matter and Accreditation Status")

#ggsave("plot3.png")

# plot 4

museums |>
  count(area_geodemographic_group, sort = T) |>
  na.omit() |>
  mutate(area_geodemographic_group = fct_reorder(area_geodemographic_group, n)) |>
  ggplot(aes(n, area_geodemographic_group, color = area_geodemographic_group)) +
  geom_point() +
  geom_errorbarh(aes(xmin = 0,
                     xmax = n),
                 height = 0,
                 size = 1) +
  theme(legend.position = "none") +
  labs(x = "count",
       y = NULL,
       title = "Museum Area of Geodemographic Group")

ggsave("plot4.png")




