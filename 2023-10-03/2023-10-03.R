library(tidyverse)
theme_set(theme_light())

grants <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grants.csv') |>
  filter(estimated_funding > 0)
grant_opportunity_details <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grant_opportunity_details.csv')

# Plot 1
grants |>
  ggplot(aes(estimated_funding)) +
  geom_histogram() +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "estimated funding",
       title = "Grant Funding Distribution")

#ggsave("plot1.png", width = 6)

# Plot 2
grants |>
  filter(!is.na(agency_code)) |>
  mutate(agency_code = str_remove_all(agency_code, "-.+"),
         agency_code = fct_reorder(fct_lump(agency_code, n = 10), estimated_funding, median)) |>
  ggplot(aes(estimated_funding, agency_code, fill = agency_code, color = agency_code)) +
  geom_boxplot(show.legend = F, alpha = 0.5) +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "estimated funding",
       y = "",
       title = "Top 10 Grants Agencies")

#ggsave("plot2.png", width = 6.5)

# Plot 3
grant_opportunity_details |>
  filter(estimated_total_program_funding > 0,
         expected_number_of_awards > 1) |>
  ggplot(aes(expected_number_of_awards, estimated_total_program_funding, color = cfda_numbers)) +
  geom_point(show.legend = F, alpha = 0.6) +
  scale_x_log10() +
  scale_y_log10(label = scales::dollar) +
  labs(x = "# of awards",
       y = "",
       title = "# of Awards and Funding") 

#ggsave("plot3.png", width = 6)


