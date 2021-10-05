library(tidyverse)
library(ggthemes)
library(patchwork)
library(geofacet)

theme_set(theme_tufte())

nurses <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv") %>%
  janitor::clean_names()

map_for_nurses <- nurses %>%
  mutate(state = str_to_lower(state)) %>%
  right_join(map_data("state"), by = c("state" = "region")) 


## Plot 1

map_for_nurses%>% 
  #filter(year == 2020) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = total_employed_rn)) +
  scale_fill_gradient(low = "grey80",
                      high = "grey20",
                      labels = comma) +
  coord_map() +
  theme_void() +
  facet_wrap(~year) +
  labs(fill = "total # of employed nurses",
       title = "Total # of Employed Nurses Per State from 1998 to 2020")

#ggsave("p1.png", width = 15, height = 6) 
  

## Plot 2

nurses %>%
  filter(year %in% c(2000, 2020),
         state != "Guam") %>%
  mutate(state = fct_reorder(state, annual_salary_median)) %>%
  ggplot(aes(y = state)) +
  geom_point(aes(x = annual_salary_median, size = total_employed_rn, color = state)) +
  geom_errorbar(aes(xmin = annual_25th_percentile,
                    xmax = annual_75th_percentile,
                    color = state), show.legend = F) +
  guides(color= "none") +
  scale_x_continuous(labels = dollar) +
  scale_size_continuous(labels = comma) +
  theme(strip.text = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 23),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 27),
        plot.caption = element_text(size = 17),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  labs(size = "total employed nurses",
       x = "annual salary",
       y = NULL,
       title = "Annual Salary Per State in 2000 & 2020",
       subtitle = "Dots refer to annual median salary, and the left end 25% percentile and the right 75% percentile.\nThe size of dots refers to total employed registered nurses",
       caption = "Currency is not adjusted") +
  facet_wrap(~year, ncol = 1)

#ggsave("p2.png", width = 20, height = 26)


## Plot 3

nurses %>%
  #filter(state != "Guam") %>%
  
  ggplot(aes(year, state, fill = hourly_wage_avg)) +
  geom_tile() +
  scale_fill_continuous(labels = dollar) +
  scale_fill_gradient2(high = "red",
                       low = "blue",
                       midpoint = 20,
                       mid = "white") +
  scale_x_continuous(breaks = seq(1998, 2020)) +
  theme(strip.text = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 23),
        plot.title = element_text(size = 30),
        plot.subtitle = element_text(size = 27),
        plot.caption = element_text(size = 17),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "year",
       y = NULL,
       fill = "average hourly wage",
       title = "State-wise Average Hourly Wage for Nurses (1998-2020)",
       subtitle = "",
       caption = "Currency is not adjusted") 

#ggsave("p3.png", width = 20, height = 22)













