library(sp)
library(tidyverse)
library(spData)
library(tmap)
library(patchwork)
#install.packages("spDataLarge",repos = "https://nowosad.github.io/drat/", type = "source")
library(spDataLarge) 

tm_shape(nz) +
  tm_fill() +
  tm_borders() +
  tm_shape(nz_elev) + tm_raster(alpha = 0.7)


world %>%
  filter(!str_detect(continent, "Antarctica|Seven seas")) %>%
  mutate(continent = fct_reorder(continent, gdpPercap, median, na.rm = T)) %>%
  ggplot(aes(gdpPercap, continent, fill = continent)) +
  geom_boxplot(show.legend = F)

# Plot 1
world %>%
  filter(!str_detect(continent, "Antarctica|Seven seas")) %>%
  arrange(desc(gdpPercap)) %>%
  ggplot(aes(lifeExp, gdpPercap, color = continent)) +
  geom_point() +
  scale_y_log10() +
  geom_text(aes(label = name_long), hjust = 1, vjust = 1, check_overlap = T) +
  geom_vline(aes(xintercept = mean(world$lifeExp, na.rm = T)), color = "blue") +
  geom_hline(aes(yintercept = mean(world$gdpPercap, na.rm = T)), color = "blue") +
  geom_vline(aes(xintercept = median(world$lifeExp, na.rm = T)), color = "red") +
  geom_hline(aes(yintercept = median(world$gdpPercap, na.rm = T)), color = "red") +
  scale_y_continuous(labels = dollar) +
  expand_limits(y = 0) +
  labs(x = "life expectancy",
       y = "GDP per capita",
       title = "What is the relation between life expectancy and GDP per capita?",
       subtitle = "Blue bars represent the respective mean, and red bars median") +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14))

#ggsave("p1.png", width = 17, height = 15) 

# Plot 2

world %>%
  as_tibble() %>%
  mutate(total_life = pop * lifeExp) %>%
  group_by(continent) %>%
  summarize(weighted_age = sum(total_life, na.rm = T)/sum(pop, na.rm = T)) %>%
  drop_na()

p21 <- world %>%
  filter(!str_detect(continent, "Antarctica|Seven seas")) %>%
  as_tibble() %>%
  ggplot(aes(lifeExp, continent, fill = continent)) +
  geom_violin(show.legend = F) +
  geom_vline(aes(xintercept = mean(world$lifeExp, na.rm = T)), color = "red", size = 1) +
  labs(x = "life expectancy",
       y = "",
       title = "Life Expectancy Across All Continents",
       subtitle = "The red bar represents the respective mean") +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14))
  


p22 <- world %>%
  filter(!str_detect(continent, "Antarctica|Seven seas")) %>%
  as_tibble() %>%
  ggplot(aes(area_km2, continent, fill = continent)) +
  geom_violin(show.legend = F) +
  scale_x_log10() +
  geom_vline(aes(xintercept = mean(world$area_km2, na.rm = T)), color = "red", size = 1) +
  labs(x = "area (square km)",
       y = "",
       title = "Country Area Across All Continents",
       subtitle = "The red bar represents the respective mean") +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14))

p21 + p22

#ggsave("p2.png", width = 17, height = 15) 

# Plot 3

world_coffee <- map_data("world") %>% 
  left_join(coffee_data, by = c("region" = "name_long")) 

p31 <- world_coffee %>%
  ggplot(aes(long, lat, group = group, fill = coffee_production_2016)) +
  geom_polygon() +
  theme_void() +
  scale_fill_gradient2(high = "green",
                       low = "red",
                       mid = "pink",
                       midpoint =  1000) +
  labs(fill = "coffee production",
       title = "Coffee Production in 2016")

p32 <- world_coffee %>%
  ggplot(aes(long, lat, group = group, fill = coffee_production_2017)) +
  geom_polygon() +
  theme_void() +
  scale_fill_gradient2(high = "green",
                       low = "red",
                       mid = "pink",
                       midpoint =  1000) +
  labs(fill = "coffee production",
       title = "Coffee Production in 2017")

p31 / p32  

#ggsave("p3.png", width = 17, height = 15) 
