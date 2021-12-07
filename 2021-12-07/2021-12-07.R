library(tidyverse)
library(countrycode)
library(widyr)
library(igraph)
library(ggraph)

spiders <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv") %>%
  mutate(distribution = str_remove(distribution, " \\(.+\\)"),
         species = str_to_title(species)) %>%
  separate_rows(distribution, sep = ", ") %>%
  mutate(distrbution = str_remove(distribution, '"'))

spiders$continent <- countrycode(spiders$distribution, origin = "country.name",destination = "continent")

## Plot 1      
map_data("world") %>%
  filter(region != "Antarctica") %>%
  left_join(spiders %>% count(distribution), by = c("region" = "distribution")) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = n)) +
  theme_void() +
  scale_fill_gradient2(low = "green",
                      high = "red",
                      midpoint = 3000,
                      mid = "pink") +
  labs(title = "Where are the spiders?",
       fill = "# of spiders") +
  theme(plot.title = element_text(size = 18))

#ggsave("plot1.png", width = 18, height = 13)





## Plot 2
spiders %>%
  mutate(family = fct_lump(family, n = 10)) %>%
  filter(family != "Other") %>%
  count(family, distribution, sort = T) %>%
  filter(n > 100) %>%
  mutate(distribution = fct_reorder(distribution, n, sum)) %>%
  ggplot(aes(n, distribution, fill = family)) +
  geom_col() +
  theme(panel.grid.major = element_blank()) +
  labs(x = "# of spiders",
       y = NULL,
       title = "Countries and spider families",
       subtitle = "Only country and family combined count more than 100 chosen") 

#ggsave("plot2.png", width = 15, height = 12)

## Plot 3

spiders %>%
  filter(!is.na(continent)) %>%
  count(author, continent, sort = T) %>%
  filter(n > 200) %>%
  mutate(author = fct_reorder(author, n, sum)) %>%
  ggplot(aes(continent, author, size = n, color = continent)) +
  geom_point(shape = 8) +
  theme(axis.text.x = element_text(size = 13),
        plot.title = element_text(size = 18)) +
  labs(x = NULL,
       y = NULL,
       size = "# of spiders",
       title = "Who found out most of spiders across various continents?") +
  scale_color_discrete(guide = "none") +
  scale_size_continuous(breaks = c(300, 500, 700, 1000)) 

#ggsave("plot3.png", width = 18, height = 11)

### Plot 4

spiders %>%
  count(year, continent, sort = T) %>%
  ggplot(aes(year, n, color = continent)) +
  geom_line() +
  geom_smooth(se = F) +
  facet_wrap(~continent) +
  theme(legend.position = "none",
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 18)) +
  labs(x = NULL,
       y = "# of spiders",
       title = "Total # of spiders among all continents from 1750 to recent")

#ggsave("plot4.png", width = 16, height = 12)

### Plot 5

spiders %>%
  count(genus, species, sort = T) %>%
  filter(n > 8) %>%
  pairwise_cor(genus, species, sort = T) %>% 
  filter(correlation != 1) %>%
  head(100) %>%
  graph_from_data_frame() %>%
  ggraph() +
  geom_edge_link(aes(alpha = correlation)) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  ggtitle("How are genus and species connected?") +
  theme(plot.title = element_text(size = 18))

#ggsave("plot5.png", width = 18, height = 12)



