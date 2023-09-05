library(tidyverse)
library(geofacet)
theme_set(theme_light())

demographics <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/demographics.csv')
wages <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/wages.csv')
states <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')

# Plot 1
demographics |>
  mutate(facet = str_remove(facet, "demographics: "),
         facet = fct_reorder(facet, -employment, sum)) |>
  filter(facet %in% c("male", "female", "college or more", "less than college")) |>
  ggplot(aes(year, employment, color = facet)) +
  geom_line() +
  labs(x = NULL,
       y = "employment (thousand)",
       color = "",
       title = "Yearly Employment Statistics")

#ggsave("plot1.png")

# Plot 2

wages |>
  group_by(year) |>
  summarize(min_union_wage = min(union_wage),
            median_union_wage = median(union_wage),
            max_union_wage = max(union_wage),
            min_nonunion_wage = min(nonunion_wage),
            median_nonunion_wage = median(nonunion_wage),
            max_nonunion_wage = max(nonunion_wage)) |>
  ungroup() |>
  ggplot(aes(x = year)) +
  geom_point(aes(y = median_union_wage, color = "union"), size = 1) +
  geom_point(aes(y = median_nonunion_wage, color = "nonunion"), size = 1) +
  geom_errorbar(aes(ymin = min_union_wage,
                    ymax = max_union_wage,
                    color = "union")) +
  geom_errorbar(aes(ymin = min_nonunion_wage,
                    ymax = max_nonunion_wage,
                    color = "nonunion")) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "",
       y = "wage",
       color = "",
       title = "Union and Nonunion Wages",
       caption = "Points refer to the median wage")

#ggsave("plot2.png")

# Plot 3
states |>
  filter(sector == "Total") |>
  ggplot(aes(year, p_members, fill = state)) +
  geom_area(alpha = 0.8, show.legend = F) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid = element_blank()) +
  facet_geo(~state) +
  labs(x = "",
       y = "% of union member workers",
       title = "Union Member Workers Per State")

#ggsave("plot3.png", width = 12, height = 6)

# Plot 4

wages |>
  filter(str_detect(facet, "demographics|sector")) |>
  mutate(facet = str_remove_all(facet, "demographics: |.+sector: ")) |>
  filter(facet != "all") |>
  select(-wage) |>
  pivot_longer(c(4:5), names_to = "wage_type", values_to = "wage") |>
  mutate(wage_type = str_remove(wage_type, "_wage")) |>
  ggplot(aes(year, wage, color = wage_type)) +
  geom_line() +
  geom_point(size = 0.5) +
  scale_y_continuous(labels = scales::dollar) + 
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~facet) +
  labs(x = "",
       y = "wage",
       color = NULL)

#ggsave("plot4.png", width = 8, height = 5)  



