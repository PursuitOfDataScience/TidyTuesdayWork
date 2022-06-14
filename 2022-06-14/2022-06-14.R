library(tidyverse)
library(lubridate)
library(geofacet)
theme_set(theme_bw())

drought <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv') %>%
  select(-c(`0`, `-9`)) %>%
  mutate(DATE = str_remove(DATE, "d_"),
         DATE = ymd(DATE),
         state = str_replace(state, "-", " ")) %>%
  pivot_longer(2:11, names_to = "dry_wet", values_to = "score") %>%
  janitor::clean_names() %>%
  mutate(decade = 10 * (year(date) %/% 10),
         state = str_to_title(state))
drought_fips <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv') %>%
  mutate(decade = 10 * (year(date) %/% 10)) 

# Plot 1

drought  %>%
  group_by(decade, state, dry_wet) %>%
  summarize(score = mean(score)) %>%
  ungroup() %>%
  filter(dry_wet %in% c("D3", "D4", "W3", "W4")) %>%
  mutate(dry_wet = recode(dry_wet, D3 = "Extreme drought"),
         dry_wet = recode(dry_wet, D4 = "Exceptional drought"),
         dry_wet = recode(dry_wet, W3 = "Extreme wet"),
         dry_wet = recode(dry_wet, W4 = "Exceptional wet")) %>%
  ggplot(aes(decade, score, fill = dry_wet)) +
  geom_area(alpha = 0.8) +
  facet_geo(~state) +
  theme(panel.grid = element_blank()) +
  labs(fill = "",
       title = "Average Drought/Wet Score per Decade") 

#ggsave("plot1.png", width = 13, height = 8)

# Plot 2

drought %>%
  filter(str_detect(dry_wet, "D")) %>%
  mutate(decade = factor(decade)) %>%
  ggplot(aes(decade, score, fill = dry_wet, color = dry_wet)) +
  geom_boxplot(alpha = 0.5) +
  scale_y_log10() +
  theme(panel.grid = element_blank()) +
  labs(y = "drought score",
       fill = "",
       color = "",
       title = "Drought Score per Decade") 

#ggsave("plot2.png", width = 13, height = 8)

# Plot 3

drought_fips %>%
  group_by(State, decade) %>%
  summarize(avg_dsci = mean(DSCI)) %>%
  ungroup() %>%
  inner_join(tibble(State = state.abb,
                    region = state.name),
             by = "State") %>%
  inner_join(
    map_data("state") %>%
      tibble() %>%
      mutate(region = str_to_title(region)),
    by = c("region")) %>%
  ggplot(aes(long, lat, group = group, fill = avg_dsci)) +
  geom_polygon() +
  facet_wrap(~decade) +
  coord_map() +
  ggthemes::theme_map() +
  scale_fill_gradient2(
    high = "red",
    low = "green",
    mid = "orange",
    midpoint = 160
  ) +
  labs(fill = "DSCI (mean)",
       title = "Average Drought Score (DSCI) per State per Decade") +
  theme(strip.text = element_text(size = 15),
        plot.title = element_text(size = 18)) 

#ggsave("plot3.png", width = 15, height = 10, bg = "white")

# Plot 4

drought_fips %>%
  distinct(State, FIPS, DSCI, decade) %>%
  ggplot(aes(DSCI, fill = factor(decade))) +
  geom_histogram(alpha = 0.5) +
  facet_geo(~State, scales = "free") +
  theme(panel.grid = element_blank()) +
  labs(fill = "decade",
       title = "DSCI Count per Decade") 

#ggsave("plot4.png", width = 21, height = 10)














  



