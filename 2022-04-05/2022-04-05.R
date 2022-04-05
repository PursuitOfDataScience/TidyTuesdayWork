library(tidyverse)
library(geofacet)
library(widyr)
library(ggraph)
theme_set(theme_bw())

news_orgs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv') %>%
  filter(country == "United States")

# Plot 1

news_orgs %>%
  separate_rows(products, sep = ", ") %>%
  filter(!is.na(products),
         !is.na(state)) %>%
  count(state, products, sort = T) %>% 
  mutate(products = fct_reorder(products, n, sum)) %>%
  ggplot(aes(n, products, fill = products)) +
  geom_col(alpha = 0.6, show.legend = F) +
  facet_geo(~state) +
  theme(panel.grid = element_blank()) +
  labs(x = "# of news agencies",
       y = NULL,
       title = "News Products Produced by News Agencies per State")

#ggsave("plot1.png", width = 13, height = 11)

# Plot 2

news_orgs %>%
  separate_rows(distribution, sep = ", ") %>%
  filter(!is.na(distribution),
         !is.na(state)) %>%
  mutate(distribution = fct_recode(distribution,
                                   "Mobile Apps" = "Owned and operated mobile app(s)",
                                   "Syndicated feed" = "Syndicated feed or content partnerships",
                                   "Third party" = "Third party content platforms"),
         distribution = str_to_title(distribution)) %>%
  count(state, distribution, sort = T) %>% 
  mutate(distribution = fct_reorder(distribution, n, sum)) %>%
  ggplot(aes(state, distribution, fill = n)) +
  geom_tile() +
  scale_fill_gradient2(
    high = "green",
    low = "purple",
    mid = "pink",
    midpoint = 15
  ) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.position = "bottom") +
  labs(x = NULL,
       y = NULL,
       fill = "# of new agencies",
       title = "The Most Popular Way to Distribute News per State")

#ggsave("plot2.png", width = 10, height = 8)

# Plot 3

set.seed(2022)
news_orgs %>%
  filter(!is.na(coverage_topics)) %>%
  separate_rows(coverage_topics, sep = ", ") %>%
  pairwise_cor(coverage_topics, publication_name, sort = T) %>%
  head(300) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(width = correlation), color = "lightblue", alpha = 0.8) +
  geom_node_point() +
  geom_node_text(aes(label = name, color = name), hjust = 0.5, vjust = 1, check_overlap = T, size = 5) +
  scale_edge_width_continuous(range = c(1,2)) +
  theme_void() +
  guides(color = "none") +
  labs(edge_width = "correlation",
       title = "How are coverage topics correlated within news agency?") +
  theme(plot.title = element_text(size = 18))

#ggsave("plot3.png", width = 10, height = 8)

# Plot 4

news_orgs %>%
  filter(!is.na(tax_status_current)) %>%
  mutate(tax_status_current = fct_lump(tax_status_current, n = 3)) %>%
  count(year_founded, tax_status_current) %>%
  ggplot(aes(year_founded, n, fill = tax_status_current)) +
  geom_area(alpha = 0.6) +
  theme(panel.grid = element_blank()) +
  labs(x = "year founded",
       y = "# of agencies",
       fill = "current tax status",
       title = "# of News Agencies Founded per Year",
       subtitle = "News agencies separated by current tax status") 

#ggsave("plot4.png", width = 10, height = 8)
  
  