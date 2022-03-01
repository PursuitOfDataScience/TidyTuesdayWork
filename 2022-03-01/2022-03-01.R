library(tidyverse)
library(geofacet)
library(scales)
library(tidytext)
library(tidylo)
theme_set(theme_light())

stations <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv") %>%
  janitor::clean_names() %>%
  filter(!state %in% c("ON", "PR")) %>%
  rename(long = x,
         lat = y) %>%
  filter(long < -50) 

stations <- stations %>%
  anti_join(stations %>%
              filter(state == "CA", long > -80),
            by = c("station_name")) 

## Plot 1

stations %>%
  ggplot(aes(long, lat, color = state)) +
  geom_point(show.legend = F) +
  #geom_text(aes(label = state), check_overlap = T, hjust = 1, vjust = 1)
  coord_fixed(1.5) +
  ggthemes::theme_map() +
  ggtitle("Where are the fuel stations located in the U.S.?") +
  theme(plot.title = element_text(size = 18))

#ggsave("plot1.png", width = 10, height = 8)


## Plot 2

stations %>%
  count(fuel_type_code, state, sort = T) %>%
  add_count(state, wt = n) %>%
  mutate(pct_station = n/nn,
         fuel_type_code = reorder_within(fuel_type_code, pct_station, state)) %>%
  ggplot(aes(pct_station, fuel_type_code, fill = fuel_type_code)) +
  geom_col(show.legend = F) +
  facet_geo(~state, scales = "free_y") +
  scale_y_reordered() +
  theme(axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 15),
        plot.title = element_text(size = 18)) +
  scale_x_continuous(labels = percent) +
  labs(x = "% of station type",
       y = "fuel type",
       title = "State-wise Fuel Station Type Percentage") 

#ggsave("plot2.png", width = 15, height = 8)

## Plot 3 

by_public <- stations %>%
  separate(groups_with_access_code, into = c("public_private", "payment_type"), sep = "\\s?\\-\\s?") %>% 
  filter(fct_lump(public_private, n = 3) != "Other") %>%
  mutate(public_private = str_to_title(public_private))

by_public %>%
  filter(public_private != "Planned") %>%
  count(state, public_private, sort = T) %>%
  arrange(state) %>%
  mutate(state = str_to_lower(state.name[match(state, state.abb)])) %>%
  left_join(map_data("state"), by = c("state" = "region")) %>%
  ggplot(aes(long, lat, group = group, fill = n)) +
  geom_polygon() +
  facet_wrap(~public_private, ncol = 1) +
  scale_fill_gradient(high = "green",
                      low = "red",
                      trans = "log10") +
  ggthemes::theme_map() +
  coord_fixed() +
  labs(fill = "# of stations",
       title = "# of Fuel Stations Per State (Public & Private)") +
  theme(strip.text = element_text(size = 15),
        plot.title = element_text(size = 18))

#ggsave("plot3.png", width = 12, height = 8)

## Plot 4

stations %>%
  mutate(facility_type = str_to_title(str_replace_all(facility_type, "_", " ")),
         access_detail_code = str_to_title(str_replace_all(access_detail_code, "_", " "))) %>%
  filter(fct_lump(facility_type, n = 9) != "Other",
         !is.na(access_detail_code)) %>%
  count(facility_type, access_detail_code, sort = T) %>%
  bind_log_odds(facility_type, access_detail_code, n) %>%
  mutate(access_detail_code = reorder_within(access_detail_code, log_odds_weighted, facility_type)) %>%
  ggplot(aes(log_odds_weighted, access_detail_code, fill = log_odds_weighted > 0)) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~facility_type, scales = "free_y") +
  theme(legend.position = "none",
        strip.text = element_text(size = 15),
        plot.title = element_text(size = 18)) +
  labs(x = "weighted log odds",
       y = "access detail code",
       title = "Weighted Log Odds of Access Detail Code within Facility Type") 

#ggsave("plot4.png", width = 13, height = 8)


