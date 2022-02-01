library(tidyverse)
library(tidytext)
theme_set(theme_light())

breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv') %>%
  janitor::clean_names()
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv') %>%
  select(-c(links, Image))


breed_pivot <- breed_rank %>%
  pivot_longer(cols = c(2:9), names_to = "year", values_to = "rank") %>%
  mutate(year = str_remove(year, " Rank$"),
         Breed = str_remove(Breed, " \\(.+\\)$")) 

# plot 1

breed_pivot %>%
  group_by(year) %>%
  slice_min(rank, n = 20) %>%
  ungroup() %>%
  mutate(year = as.numeric(year),
         Breed = reorder_within(Breed, -rank, year, sum)) %>%
  ggplot(aes(rank, Breed, fill = rank)) +
  geom_col(show.legend = F) +
  scale_y_reordered() +
  scale_x_continuous(breaks = seq(1,20,2)) +
  facet_wrap(~year, ncol = 4, scales = "free_y") +
  scale_fill_gradient(low = "gold", high = "brown") +
  theme(strip.text = element_text(size = 15),
        plot.title = element_text(size = 17)) +
  labs(y = "",
       title = "Yearly Top 20 Ranking Breed")

#ggsave("plot1.png", width = 18, height = 11)

# Plot 2

breed_traits %>%
  pivot_longer(cols = is.numeric, names_to = "metric") %>%
  group_by(coat_type, coat_length, metric) %>%
  summarize(mean_score = mean(value)) %>%
  ungroup() %>%
  filter(mean_score > 0) %>%
  mutate(metric = str_replace_all(metric, "_", " ")) %>%
  ggplot(aes(coat_type, metric, fill = mean_score)) +
  geom_tile() +
  facet_wrap(~coat_length, scale = "free_x") +
  theme(strip.text = element_text(size = 15),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18)) +
  labs(x = "coat type",
       y = "",
       fill = "mean score",
       title = "Average Score between Coat Type and Coat Length Across Metrics") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient(low = "red", high = "green") 

#ggsave("plot2.png", width = 18, height = 11)

# Plot 3

breed_traits %>%
  select(breed, contains("good"), barking_level, affectionate_with_family) %>%
  filter(barking_level > 0, good_with_young_children > 0) %>%
  ggplot(aes(good_with_young_children, good_with_other_dogs)) +
  geom_jitter(aes(color = affectionate_with_family, size = barking_level)) +
  geom_text(aes(label = breed), check_overlap = T, hjust = 1, vjust = 1) +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18)) +
  labs(x = "good with kids",
       y = "good with dog peers",
       size = "barking level",
       color = "good with family",
       title = "The Relations between Dogs and Good with Other Beings") +
  scale_color_gradient(high = "green", low = "red")

#ggsave("plot3.png", width = 15, height = 11)

# plot 4

trait_description %>%
  unnest_tokens(word, Description) %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("dogs", "day", "breeds")) %>%
  count(Trait, word, sort = T) %>%
  filter(n > 1) %>%
  bind_tf_idf(word, Trait, n) %>%
  mutate(word = fct_reorder(word, tf_idf, sum)) %>%
  ggplot(aes(tf_idf, word, fill = Trait)) +
  geom_col(position = "dodge") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18)) +
  labs(x = "TF-IDF",
       fill = "trait",
       title = "The Largest TF-IDF Description Words Across Traits")

#ggsave("plot4.png", width = 15, height = 11)









