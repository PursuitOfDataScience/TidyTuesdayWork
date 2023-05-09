library(tidyverse)
library(scales)
library(maps)
library(geofacet)

data(county.fips)
theme_set(theme_light())
childcare_costs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/childcare_costs.csv')

# data cleaning
counties <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/counties.csv') |>
  rename(county = county_name,
         state = state_name) |>
  mutate(county = str_to_lower(str_remove(county, " County")),
         state = str_to_lower(state))

# data joining
county_joined <- counties |>
  right_join(childcare_costs, by = "county_fips_code", multiple = "all") |>
  mutate(county = str_replace(county, "\\.", ""),
         county = str_replace_all(county, " ", ""),
         county = str_remove(county, "parish")) |>
  rename(fips = county_fips_code)

# Plot 1

map_data("county") |>
  mutate(polyname = paste(region,subregion,sep=",")) |>
  inner_join(county.fips, by="polyname") |>
  tibble() |>
  left_join(county_joined |>
              mutate(fips = as.integer(fips)) |>
              filter(study_year %in% c(2008, 2010, 2014, 2018)),
            by = "fips") |>
  ggplot(aes(long, lat, group = group, fill = unr_16)) +
  geom_polygon() +
  coord_map() +
  scale_fill_viridis_c(label = percent_format(scale = 1)) +
  theme_void() +
  facet_wrap(~study_year) +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 10, face = "bold")) +
  labs(fill = "unemployment rate",
       title = "Unemployment Rate in 4 Study Years",
       subtitle = "The population aged 16 years old or older")

#ggsave("plot1.png", width = 10, height = 6, bg = "white")

# Plot 2

county_joined |>
  group_by(state_abbreviation, study_year) |>
  summarize(Female = median(fme_2018),
            Male = median(mme_2018)) |>
  ungroup() |>
  pivot_longer(3:4) |>
  mutate(name = fct_reorder(name, -value)) |>
  ggplot(aes(study_year, value, color = name)) +
  geom_line() +
  facet_geo(~state_abbreviation) +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)) +
  scale_color_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 90,
                                   size = 6)) +
  labs(x = NULL,
       y = "income",
       color = NULL,
       title = "Annual Median Earnings For Men and Women Per State")

#ggsave("plot2.png", width = 10, height = 8, bg = "white")

# Plot 3
county_joined |>
  select(state_abbreviation, study_year, contains("race")) |>
  select(-one_race) |>
  rename(Asian = one_race_a,
         Black = one_race_b,
         Hawaiian = one_race_h,
         Indian = one_race_i,
         White = one_race_w,
         Other = one_race_other,
         `Two Races` = two_races) |>
  pivot_longer(3:9) |>
  mutate(decade = 2 * study_year %/% 2) |>
  group_by(state_abbreviation, decade, name) |>
  summarize(value = mean(value), .groups = "drop") |>
  ggplot(aes(value, state_abbreviation, fill = name)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~decade) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  theme(axis.text.y = element_text(size = 5)) +
  labs(x = NULL,
       y = NULL,
       fill = NULL,
       title = "Average Race Percentage Per State Per Two Years")

#ggsave("plot3.png", width = 10, height = 7, bg = "white")

# Plot 4

county_joined |>
  select(state_abbreviation, study_year, total_pop, households) |>
  group_by(state_abbreviation, study_year) |>
  summarize(total_pop = sum(total_pop), 
            households = sum(households),
            .groups = "drop") |>
  ggplot(aes(total_pop, households)) +
  geom_point(alpha = 0.2) +
  geom_text(aes(label = state_abbreviation), 
            check_overlap = T,
            size = 2,
            vjust = 1,
            hjust = 1) +
  facet_wrap(~study_year) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "population",
       y = "# of households",
       title = "Total State Population and # of Households Per Study Year")

#ggsave("plot4.png", width = 8)







