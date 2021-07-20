library(tidyverse)
library(lubridate)
library(patchwork)
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')
stateabb <- read_csv("stateabb.csv")

# add year column to the tibble

drought <- drought %>% 
  mutate(
    year = year(valid_start),
    month = month(valid_start)
  )

states_map <- map_data("state")
stateabb <- stateabb %>% select(State, Code)


map_fun <- function(cal_year){
drought %>% filter(year == cal_year, month %in% c(5,6,7)) %>%
  group_by(year, state_abb, drought_lvl) %>%
  summarize(`Average Population Affected (%)` = mean(pop_pct)) %>%
  filter(drought_lvl != "None") %>%
  ungroup() %>%
  left_join(stateabb, by = c("state_abb" = "Code")) %>%
  mutate(State = tolower(State)) %>%
  ggplot(aes(map_id = State)) +
  geom_map(aes(fill = `Average Population Affected (%)`), map = states_map) +
  #facet_grid(rows = vars(year)) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title = element_blank(),
    plot.title = element_text(size = 18),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 15)
  ) +
  ggtitle(paste("Average Summertime (5, 6, 7) Population Percentage Affected By Drought", cal_year))

}

map_fun(2018) + map_fun(2019) + map_fun(2020) + map_fun(2021) + plot_layout(nrow = 2, byrow = FALSE)

#ggsave("Average Population Affected.png", width = 20, height = 15)
