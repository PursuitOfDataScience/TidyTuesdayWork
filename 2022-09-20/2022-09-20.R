library(tidyverse)
theme_set(theme_light())

hw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-20/HydroWASTE_v10.csv') |>
  janitor::clean_names() |>
  mutate(qual_loc = case_when(qual_loc == 1 ~ "high",
                              qual_loc == 2 ~ "medium",
                              qual_loc == 3 ~ "low",
                              qual_loc == 4 ~ "unknown"))


# Plot 1
hw |>
  ggplot(aes(lon_wwtp, lat_wwtp, color = level)) +
  geom_point(alpha = 0.3, size = 0.5) +
  borders("world") + 
  guides(color = guide_legend(override.aes = list(alpha = 1,
                                                  size = 3))) +
  theme_void() +
  labs(title = "Wastewater Plants and Levels")

#ggsave("plot1.png", bg = "white")


# Plot 2 

hw |>
  add_count(country) |>
  filter(n > 1250,
         pop_served > 0,
         waste_dis > 0) |>
  ggplot(aes(pop_served, waste_dis, color = country)) +
  geom_point(alpha = 0.3, show.legend = F) +
  facet_wrap(~country) +
  scale_y_log10(labels = scales::comma) +
  scale_x_log10(labels = scales::comma) +
  labs(x = "population served",
       y = "waste disposal",
       title = " Top 12 Countries and Their Wastewater Plants")

#ggsave("plot2.png")


# Plot 3

hw |>
  add_count(wwtp_name) |>
  filter(!is.na(wwtp_name)) |>
  slice_max(pop_served, n = 20, with_ties = F) |> 
  filter(n == 1) |>
  mutate(wwtp_name = paste0(wwtp_name, " (", country, ")"),
         wwtp_name = fct_reorder(wwtp_name, pop_served)) |>
  
  ggplot(aes(pop_served, wwtp_name, fill = qual_loc)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "population served",
       y = NULL,
       fill = "quality",
       title = "20 Largest Population Served Wastewater Plants")

#ggsave("plot3.png", width = 8)

# Plot 4

hw |>
  mutate(qual_waste = case_when(
    qual_waste == 1 ~ "treated",
    qual_waste == 2 ~ "design capacity",
    qual_waste == 3 ~ "not identified",
    qual_waste == 4 ~ "estimated"
  )) |>
  ggplot(aes(waste_dis, qual_waste, fill = qual_waste, color = qual_waste)) +
  geom_violin(show.legend = F, alpha = 0.5) +
  scale_x_log10(labels = scales::comma) +
  labs(x = "waste disposal",
       y = NULL,
       title = "Waste Disposal Types and Volumes") 

#ggsave("plot4.png")












