library(tidyverse)
library(patchwork)
library(scales)
theme_set(theme_light())

eggproduction  <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/egg-production.csv') 

cagefreepercentages <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/cage-free-percentages.csv')

eggpro <- eggproduction |>
  filter(prod_process != "all") |>
  mutate(prod_process = str_remove_all(str_extract(prod_process, "\\(.+\\)"), "\\(|\\)")) 

# Plot 1
p11 <- eggproduction |>
  ggplot(aes(n_hens, n_eggs, color = prod_process)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(color = "prod process",
       x = "# of hens",
       y = "# of eggs",
       title = "# of Hens and Eggs Per Producation Process")

p12 <- eggproduction |>
  pivot_longer(cols = c(n_hens, n_eggs)) |>
  ggplot(aes(observed_month, value, color = name)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~prod_process, ncol = 1) +
  labs(x = NULL,
       y = "count",
       color = NULL,
       title = "Monthly Hens and Eggs Count")

p11 / p12

#ggsave("plot1.png", width = 7, height = 6)

# Plot 2

eggproduction |>
  pivot_longer(cols = c(n_hens, n_eggs)) |>
  mutate(name = ifelse(name == "n_hens", "# of hens", "# of eggs")) |>
  ggplot(aes(value, prod_type, fill = prod_process)) +
  geom_violin() +
  scale_x_log10() +
  facet_wrap(~name,scales = "free_x") +
  labs(x = "count",
       y = "",
       fill = "prod process",
       title = "The Violin Plot for Egg and Hen Counts")

#ggsave("plot2.png", width = 7, height = 4)

# Plot 3

p31 <- cagefreepercentages |>
  pivot_longer(2:3, names_to = "pct") |>
  mutate(pct = str_remove(pct, "percent_")) |>
  ggplot(aes(observed_month, value, color = pct)) +
  geom_line(alpha = 0.8) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(x = NULL,
       y = "cage-free percentage",
       color = NULL,
       title = "Percentage of Cage-Free Eggs and Hens over Time") 

p32 <- cagefreepercentages |>
  ggplot(aes(percent_hens, percent_eggs)) +
  geom_point() +
  expand_limits(x = 0,
                y = 0) +
  labs(x = "hens (%)",
       y = "eggs (%)",
       title = "Cage-Free Eggs and Hens Percentage")


p31 / p32

ggsave("plot3.png", width = 7, height = 5)


