library(tidyverse)
library(ggthemes) 
library(patchwork)
library(tidytext)
theme_set(theme_tufte())


emmy <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-21/nominees.csv")


top_6_distributor <-
  emmy %>%
  count(distributor, sort = T) %>%
  select(distributor) %>%
  head(6) %>%
  pull() 

# Plot 1
p11 <- emmy %>%
  filter(distributor %in% top_6_distributor,
         type == "Winner") %>%
  count(year, distributor, sort = T) %>%
  ggplot(aes(year, n, color = distributor)) +
  geom_line(size = 1.5, show.legend = F) +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 15, face = "bold"),
    plot.title = element_text(size = 18)
  ) +
  facet_wrap(~distributor) +
  labs(y = "# of awards",
       title = "# of Yearly Emmy Awards From Top 6 Distributors",
       subtitle = "Top 6 distributors computed based on total # of nominees received")

p12 <- emmy %>%
  #filter(type == "Winner") %>%
  count(type, year, sort = T) %>%
  ggplot(aes(year, n, color = type)) +
  geom_line(size = 2) +
  scale_x_continuous(breaks = seq(1960, 2020, 5)) +
  #scale_y_continuous(breaks = seq(0, 500, 50)) +
  labs(x = NULL,
       y = "# of Emmy Awards",
       title = "Yearly # of Emmy Nominees & Winners",
       caption = "Before 2005, almost all nominees were winners, yet there were much more nominees after that besides the year of 2014") +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 15, face = "bold"),
    plot.title = element_text(size = 18),
    plot.caption = element_text(size = 13, color = "blue")
  )

p11 / p12

#ggsave("1.png", width = 20, height = 10)   

# Plot 2

emmy %>%
  filter(!is.na(production)) %>%
  mutate(production = str_remove(production, ", .+")) %>%
  group_by(type, production) %>% 
  summarize(count = n()) %>%
  slice_max(count, n = 10) %>%  
  mutate(production = reorder_within(production, count, type)) %>%
  ggplot(aes(count, production, fill = production)) +
  geom_col(show.legend = F) +
  facet_wrap(~type, scales = "free_y") +
  scale_y_reordered() +
  labs(x = "# of times being nominated/winning",
       title = "Top 10 Production Nominees & Winners") +
  theme(
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 15, face = "bold"),
    plot.title = element_text(size = 18)
  )
  
#ggsave("2.png", width = 20, height = 10) 

# Plot 3

emmy %>%
  group_by(title, type) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(type) %>%
  slice_max(count, n = 10) %>%
  mutate(title = reorder_within(title, count, type)) %>%
  ggplot(aes(count, title)) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~type, scales = "free_y") +
  labs(y = NULL,
       title = "Top 10 Nominees & Winners") +
  theme(
    strip.text = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 18),
    axis.ticks = element_blank()
  )

#ggsave("3.png", width = 20, height = 10) 



