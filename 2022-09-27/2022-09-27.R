library(tidyverse)
library(geofacet)
library(scales)
library(patchwork)
theme_set(theme_light())

artists <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv') |>
  group_by(state, race) |>
  summarize(across(all_workers_n:artists_n, sum, na.rm = T), .groups = "drop") |>
  group_by(state) |>
  mutate(sum_all_workers = sum(all_workers_n),
         sum_artists = sum(artists_n),
         pct_all_workers = all_workers_n/sum_all_workers,
         pct_artists = artists_n/sum_artists) |>
  ungroup() 


# Plot 1

artists |>
  ggplot(aes(pct_artists, race, fill = race)) +
  geom_col() +
  scale_x_continuous(labels = percent) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  facet_geo(~state) +
  labs(x = "pct of artists",
       y = NULL,
       title = "Percentage of Artists Per Race Per State") 

#ggsave("plot1.png", height = 8, width = 12)


# Plot 2

artists |>
  ggplot(aes(pct_all_workers, race, color = race)) +
  geom_point() +
  geom_errorbarh(aes(xmin = 0,
                     xmax = pct_all_workers),
                 height = 0) +
  scale_x_continuous(labels = percent) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  facet_geo(~state) +
  labs(x = "pct of workers",
       y = NULL,
       title = "Percentage of Workers Per Race Per State") 

#ggsave("plot2.png", height = 8, width = 12)

# Plot 3

p_worker <- artists |>
  ggplot(aes(all_workers_n, race, fill = race, color = race)) +
  geom_violin(alpha = 0.6) +
  scale_x_log10(labels = comma) +
  theme(legend.position = "none") +
  labs(x = "# of workers",
       y = NULL,
       title = "# of Workers Per Race")


p_artist <- artists |>
  filter(artists_n > 0) |>
  ggplot(aes(artists_n, race, fill = race, color = race)) +
  geom_violin(alpha = 0.6) +
  scale_x_log10(labels = comma) +
  theme(legend.position = "none") +
  labs(x = "# of workers",
       y = NULL,
       title = "# of Artists Per Race")

p_worker / p_artist

#ggsave("plot3.png")

# Plot 4

artists |>
  filter(artists_n > 0) |>
  ggplot(aes(artists_n, all_workers_n, color = race)) +
  geom_point(alpha = 0.7) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  labs(x = "# of artists",
       y = "# of workers",
       color = NULL,
       title = "# of Artists and # of Workers") 

#ggsave("plot4.png")

  
