library(tidyverse)
library(widyr)
library(ggraph)
theme_set(theme_light())

scurvy <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-25/scurvy.csv')

scurvy_long <- scurvy |>
  pivot_longer(cols = c(4:7), names_to = "symptom", values_to = "severity") |>
  mutate(symptom = str_replace_all(str_remove(symptom, "_d6"), "_", " "),
         severity = str_remove(severity, "^._"),
         treatment = str_replace_all(treatment, "_", " "),
         severity = factor(severity, levels = c("none", "mild",
                                                  "moderate",
                                                  "severe"))) 
# plot 1
scurvy_long |>
  count(treatment, severity, symptom, sort = T)  |>
  ggplot(aes(severity, treatment, fill = n)) +
  geom_tile() +
  facet_wrap(~symptom) +
  scale_fill_continuous(low = "lightgreen",
                        high = "darkgreen",
                        breaks = c(1,2)) +
  theme(panel.grid = element_blank()) +
  labs(fill = "count",
       y = "",
       title = "Severity, Symptoms, and Treatment For Scurvy",
       subtitle = "It seems citrus is effective for curing scurvy.")

#ggsave("plot1.png")

# plot 2
scurvy |>
  count(treatment, fit_for_duty_d6) |>
  mutate(fit_for_duty_d6 = str_remove_all(fit_for_duty_d6, "^._"),
         treatment = str_replace_all(treatment, "_", " ")) |>
  ggplot(aes(n, treatment, fill = fit_for_duty_d6)) +
  geom_col() +
  scale_x_continuous(breaks = c(0, 1, 2)) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "patient count",
       fill = "fit for duty")

#ggsave("plot2.png")

# plot 3
scurvy_long |>
  mutate(symptom = ifelse(symptom == "weakness of the knees", "knees weakness", symptom)) |>
  count(symptom, severity) |>
  pairwise_cor(symptom, severity, value = n) |>
  ggraph(layout = "fr") +
  geom_edge_link(aes(color = correlation), width = 1.5) +
  geom_node_point() +
  geom_node_text(aes(label = name, color = name), hjust = 0, vjust = 1, check_overlap = T, size = 3) +
  scale_edge_width_continuous(range = c(1,2)) +
  scale_edge_color_continuous(low = "red",
                              high = "darkred") +
  
  theme_void() +
  theme(legend.position = "bottom") +
  guides(color = "none") +
  labs(edge_width = "# of transports",
       title = "Pearson's Correlation Among Scurvy Symptoms") 

ggsave("plot3.png", bg = "white", width = 10)  





