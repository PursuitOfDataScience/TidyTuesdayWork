library(tidyverse)
library(patchwork)
theme_set(theme_light())

species <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/species.csv')
surveys <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/surveys.csv')

# Plot 1

surveys |>
  count(year, treatment) |>
  ggplot(aes(year, n, color = treatment)) +
  geom_point() +
  geom_line() +
  labs(x = NULL,
       y = "rodent count",
       title = "Rodent Count Surveyed Per Treatment Per Year")

#ggsave("plot1.png", width = 6)

# Plot 2

surveys |>
  select(sex, hfl, wgt, species) |>
  rename(`hindfoot length` = hfl,
         weight = wgt) |>
  pivot_longer(cols = 2:3) |>
  na.omit() |>
  mutate(species = fct_lump(species, n = 3)) |>
  ggplot(aes(sex, value, fill = species, color = species)) +
  geom_boxplot(alpha = 0.5) +
  facet_wrap(~name, ncol = 1) +
  scale_y_log10() +
  theme(legend.position = "bottom") +
  labs(title = "Hindfoot Length and Weight For Male and Feamle Species")

#ggsave("plot2.png")

# Plot 3

p31 <- species |>
  filter(!is.na(species)) |>
  mutate(species = fct_reorder(species, meanhfl)) |>
  ggplot(aes(y = species)) +
  geom_point(aes(x = meanhfl)) +
  geom_errorbarh(aes(xmin = minhfl,
                     xmax = maxhfl),
                 height = 0.5) +
  labs(x = "hindfoot length",
       title = "Hindfoot Length and Weidth Per Species",
       subtitle = "Min, Mean, and Max values are shown")

p32 <- species |>
  filter(!is.na(species)) |>
  mutate(species = fct_reorder(species, meanwgt)) |>
  ggplot(aes(y = species)) +
  geom_point(aes(x = meanwgt)) +
  geom_errorbarh(aes(xmin = minwgt,
                     xmax = maxwgt),
                 height = 0.5) +
  scale_x_log10() +
  labs(x = "weight",
       y = NULL)

p31 + p32

#ggsave("plot3.png", width = 8)

# Plot 4

species |>
  filter(juvwgt > 0) |>
  mutate(scientificname = str_to_title(scientificname),
         scientificname = paste0(scientificname, " (", species, ")"),
         scientificname = fct_reorder(scientificname, juvwgt)) |>
  ggplot(aes(juvwgt, scientificname, color = scientificname,
         fill = scientificname)) +
  geom_col(show.legend = F, alpha = 0.7) +
  labs(x = "juvenile weight",
       y = NULL,
       title = "Juvenile Weight Per Species with Scientific Name")

#ggsave("plot4.png", width = 7)



