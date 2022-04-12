library(tidyverse)
library(janitor)
library(scales)
library(worlddatajoin)
library(ggthemes)
theme_set(theme_bw())

tuesdata <- tidytuesdayR::tt_load('2022-04-12')

fuel_gdp <- tuesdata$fuel_gdp %>%
  clean_names()

fuel_access <- tuesdata$fuel_access %>%
  clean_names()

indoor_pollution <- tuesdata$indoor_pollution %>%
  clean_names()




# Plot 1

indoor_pollution %>%
  filter(entity %in% c("Africa", "Asia", "America", "Europe", "Oceania")) %>%
  mutate(entity = if_else(entity == "America", "Americas", entity)) %>%
  rename(pct_death_air_pollution = 4) %>%
  mutate(entity = fct_reorder(entity, -pct_death_air_pollution, last)) %>%
  ggplot(aes(year, pct_death_air_pollution, color = entity)) +
  geom_line() +
  geom_point() +
  theme(panel.grid = element_blank()) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  labs(x = NULL,
       y = "deaths %",
       color = NULL,
       title = "Percentage of Deaths Caused by Household Air Pollution")

#ggsave("plot1.png")


# Plot 2

world_map_2010 <- worlddatajoin::world_data(year = 2010) 

fuel_access %>%
  rename(pct_clean_fuels = 4) %>%
  right_join(
    world_map_2010 %>% select(-year),
    by = c("code" = "iso3c")
  ) %>%
  filter(year %in% c(2000, 2005, 2010, 2016)) %>%
  ggplot(aes(long, lat, group = group, fill = pct_clean_fuels)) +
  geom_polygon() +
  facet_wrap(~year) +
  scale_fill_gradient(
    low = "red",
    high = "green",
    labels = percent_format(scale = 1)
  ) +
  theme_map() +
  theme(
    strip.text = element_text(size = 15),
    plot.title = element_text(size = 17)
  ) +
  labs(fill = "clean fuels access",
       title = "World Maps for the Percentage of Clean Fuel Acess") 

#ggsave("plot2.png", width = 8, height = 7)  

# Plot 3


fuel_gdp  %>%
  filter(year %in% c(2000, 2010),
         !is.na(code)) %>%
  rename(access_to_clean_fuels = 4,
         gdp_per_capita = 5,
         population = population_historical_estimates) %>%
  ggplot(aes(gdp_per_capita, access_to_clean_fuels)) +
  geom_point(aes(size = population),
             alpha = 0.5) +
  geom_text(aes(label = entity),
            vjust = 1,
            hjust = 1,
            check_overlap = T) +
  facet_wrap(~year) +
  scale_x_log10(labels = dollar) +
  scale_y_log10(labels = percent_format(scale = 1)) +
  theme(panel.grid = element_blank()) +
  labs(x = "GDP per capita",
       y = "access % to clean fuels",
       title = "Access to Clean Fuels VS GDP per capita") 

#ggsave("plot3.png", width = 10, height = 7)    

# Plot 4

indoor_pollution %>%
  filter(code %in% c("USA", "CHN", "JPN", "GBR")) %>%
  rename(pct_death_air_pollution = 4) %>%
  ggplot(aes(year, pct_death_air_pollution, fill = entity)) +
  geom_col(show.legend = F,
           alpha = 0.7) +
  facet_wrap(~entity, scales = "free_y") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  theme(strip.text = element_text(size = 15),
        plot.title = element_text(size = 18),
        panel.grid = element_blank()) +
  labs(x = NULL,
       y = "deaths %",
       color = NULL,
       title = "Percentage of Deaths Caused by Household Air Pollution")

#ggsave("plot4.png", width = 10, height = 7)    




  
