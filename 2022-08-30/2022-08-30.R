library(tidyverse)
library(geofacet)
theme_set(theme_light())

pell <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv') |>
  janitor::clean_names()

# Plot 1

pell |>
  group_by(state, year) |>
  summarize(avg_award = sum(award)/sum(recipient),
            .groups = "drop") |>
  ggplot(aes(year, avg_award, fill = state)) +
  geom_area(show.legend = F, alpha = 0.7) +
  scale_y_continuous(labels = scales::dollar) +
  theme(axis.text.x= element_text(angle = 90)) +
  facet_geo(~state) +
  labs(x = NULL,
       y = "yearly mean award",
       title = "Average Pell Award Amount Per Person Per State Per Year")

#ggsave("plot1.png", height = 8, width = 9)


# Plot 2

pell |>
  group_by(year) |>
  summarize(n = n(),
            first_quantile_award = quantile(award, 0.25, na.rm = T),
            median_award = quantile(award, 0.5, na.rm = T),
            third_quantile_award = quantile(award, 0.75, na.rm = T),
            .groups = "drop") |> 
  ggplot(aes(x = year, y = median_award)) +
  geom_line() +
  geom_point(aes(size = n)) +
  geom_ribbon(aes(ymin = first_quantile_award,
                  ymax = third_quantile_award),
              alpha = 0.5,
              fill = "midnightblue") +
  scale_y_log10(labels = scales::dollar) +
  scale_size_continuous(range = c(1,4)) +
  labs(x = NULL,
       y = "award amount",
       size = "total # of recipents",
       title = "Yearly Awawd 25%, Median, 75% Quantile Amount")  

#ggsave("plot2.png")

# Plot 3

pell |>
  mutate(avg_award = award / recipient) |>
  group_by(year) |>
  slice_max(avg_award, n = 1, with_ties = F) |>
  ungroup() |>
  mutate(name_year = paste0(name, "(", year, ")"),
         name_year = fct_reorder(name_year, year)) |>
  ggplot(aes(avg_award, name_year, color = name_year)) +
  geom_point(aes(size = recipient)) +
  geom_errorbarh(aes(xmin = 0,
                     xmax = avg_award),
                 height = 0) +
  scale_x_continuous(labels = scales::dollar) +
  scale_color_discrete(guide = "none") +
  scale_size_continuous(range = c(2,5)) +
  theme(panel.grid.major.x = element_blank()) +
  labs(x = "average award per person",
       y = NULL,
       size = "recipients",
       title = "The Largest Average Award Amount Per Year")

#ggsave("plot3.png", width = 8)

# Plot 4

pell |>
  filter(award > 0) |>
  ggplot(aes(year, recipient, group = year, color = factor(year), fill = factor(year))) +
  geom_boxplot(alpha = 0.6, show.legend = F) +
  scale_y_log10(labels = scales::comma) +
  labs(x = NULL,
       y = "# of recipients",
       title = "Yearly Pell Award Recipients") 

ggsave("plot4.png")
  




