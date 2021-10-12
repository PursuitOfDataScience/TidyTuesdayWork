library(tidyverse)
library(janitor)
library(countrycode)
library(tidytext)
library(ggthemes)
library(scales)
theme_set(theme_tufte())

farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/aquaculture-farmed-fish-production.csv')
captured_vs_farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv')
captured <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fishery-production.csv')
consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-and-seafood-consumption-per-capita.csv')
stock <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-stocks-within-sustainable-levels.csv')
fishery <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/global-fishery-catch-by-sector.csv')
production <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')


# working on farmed 

# Plot 1
farmed %>%
  clean_names() %>% 
  left_join(raster::ccodes(), by = c(code = "ISO3")) %>% 
  mutate(decade = 10 * floor(year/10)) %>%
  group_by(continent, decade) %>%
  summarize(sum_prod = sum(aquaculture_production_metric_tons, na.rm = T)) %>%
  ungroup() %>% 
  filter(!is.na(continent)) %>%
  mutate(continent = reorder_within(continent, sum_prod, decade)) %>%
  ggplot(aes(sum_prod, continent, fill = continent)) +
  geom_col(show.legend = F) +
  facet_wrap(~decade, scales = "free") +
  scale_y_reordered() +
  labs(x = "Aquaculture production (metric tons)",
       y = NULL,
       title = "Decade-wise Aquaculture Production from All Continents") +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 13),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size = 18)
  )

#ggsave("p1.png", width = 15, height = 6) 

# Plot 2

captured_vs_farmed %>%
  clean_names() %>%
  mutate(decade = 10 * floor(year/10)) %>%
  group_by(entity, decade) %>%
  summarize_at(vars(contains("tons")), sum, na.rm = T) %>%
  ungroup() %>%
  mutate(ratio_farm_to_capture = aquaculture_production_metric_tons/(capture_fisheries_production_metric_tons + aquaculture_production_metric_tons)) %>%
  group_by(decade) %>%
  slice_max(ratio_farm_to_capture, n = 30) %>%
  ungroup() %>%
  complete(decade, entity, fill = list(ratio_farm_to_capture = 0)) %>%
  ggplot(aes(decade, entity, fill = ratio_farm_to_capture)) +
  geom_tile() +
  scale_fill_gradient2(low = "red",
                      high = "blue",
                      midpoint = 0.1,
                      mid = "pink",
                      labels = percent) +
  scale_x_continuous(breaks = seq(1960, 2010, 10), expand = c(0,0)) +
  labs(y = NULL,
       fill = "farmed ratio",
       title = "Top 30 Entities within each Decade on the Farmed Production Ratio",
       subtitle = "farmed ratio is computed by using farmed production divides by total production within each decade in each entity")

#ggsave("p2.png", width = 15, height = 9) 
# Plot 3

fishery %>% 
  clean_names() %>%
  pivot_longer(cols = -c("entity", "code", "year"), names_to = "sector", values_to = "value") %>%
  mutate(sector = str_replace_all(sector, "_", " "),
         sector = fct_reorder(sector, -value, sum)) %>%
  ggplot(aes(year, value, color = sector)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(1950, 2010, 5)) +
  labs(y = "metric tons",
       title = "Global Fishery Catch by Sector") +
  theme(
    plot.title = element_text(size= 18),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13),
    axis.title.y = element_text(size = 15)
  )

#ggsave("p3.png", width = 15, height = 6) 











