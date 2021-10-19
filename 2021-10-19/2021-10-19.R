library(tidyverse)
library(ggthemes)
library(tidytext)
library(geofacet)
theme_set(theme_tufte())

pumpkin_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv")

pumpkin <- pumpkin_raw %>%
  separate(id, into = c("year", "id")) %>%
  filter(!str_detect(country, ",")) %>%
  mutate(weight_lbs = as.numeric(weight_lbs)) %>%
  filter(!(state_prov == "Virginia" & country == "Australia")) %>%
  mutate(id = fct_recode(id, 
                         "Field Pumpkin" = "F",
                         "Giant Pumpkin" = "P",
                         "Giant Squash" = "S",
                         "Giant Watermelon" = "W", 
                         "Long Gourd" = "L",
                         "Tomato" = "T"))

# Plot 1

pumpkin %>%
  ggplot(aes(weight_lbs, year)) +
  geom_boxplot(aes(fill = year), show.legend = F) 
  
pumpkin %>%
  group_by(year, state_prov) %>%
  summarize(avg_weight = mean(weight_lbs, na.rm = T)) %>%
  slice_max(avg_weight, n = 10) %>%
  ungroup() %>%
  inner_join(
    pumpkin %>% 
      select(state_prov, country), by = "state_prov"
  ) %>%
  distinct() %>%
  mutate(state_prov = reorder_within(state_prov, avg_weight, year)) %>%
  ggplot(aes(avg_weight, state_prov, fill = country)) +
  geom_col() +
  facet_wrap(~year, scales = "free") +
  scale_y_reordered() +
  theme(strip.text = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18)) +
  labs(x = "average pumpkin weight",
       y = "",
       title = "Top 10 Yearly Average Pumpkin Weight States/Provinces Colored by Country")

#ggsave("p1.png", width = 15, height = 8) 

### Plot 2
pumpkin %>%
  filter(country != "Unknown country") %>%
  group_by(year, id, country) %>%
  summarize(avg_weight = mean(weight_lbs, na.rm = T)) %>%
  ungroup() %>%
  complete(id, year, country, fill = list(avg_weight = 0)) %>%
  ggplot(aes(year, country, fill = avg_weight)) +
  geom_tile() +
  facet_wrap(~id, scales = "free") +
  scale_fill_gradient2(
    high = "green",
    low = "red",
    mid = "white",
    midpoint = 400
  ) +
  theme(strip.text = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18)) +
  labs(x = "",
       y = "",
       fill = "average weight",
       title = "Country-wise Yearly Average Weight of Various Kinds of Pumpkins")

#ggsave("p2.png", width = 15, height = 8) 

### Plot 3

pumpkin %>%
  filter(country == "United States") %>% 
  group_by(year, id, state_prov) %>%
  summarize(avg_weight = mean(weight_lbs, na.rm = T)) %>%
  ungroup() %>% 
  ggplot(aes(as.numeric(year), avg_weight, color = id)) +
  geom_line(size = 1) +
  facet_geo(~state_prov, label = "name", scales = 'free')+
  scale_x_continuous(breaks = seq(2013, 2021, 3)) +
  labs(x = "year",
       y = "average weight",
       color = "pumpkin type",
       title = "State-wise Pumpkin Average Weight")+
  theme(strip.text = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18)) 

#ggsave("p3.png", width = 22, height = 10) 

 