library(tidyverse)
library(tidytext)

theme_set(theme_bw())

lyrics <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/lyrics.csv")
artists <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/related_artists.csv")
tracks <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/studio_album_tracks.csv")


# Plot 1

artists %>%
  distinct(artist_id, artist_name, popularity, followers_total) %>%
  ggplot(aes(popularity, followers_total, color = artist_name)) +
  geom_point() +
  geom_smooth(aes(group = 1), method = "lm", se = F) +
  geom_text(aes(label = artist_name), hjust = 1, vjust = 1, check_overlap = T) +
  scale_y_log10() +
  theme(legend.position = "none") +
  labs(y = "followers in total",
       title = "What is the relationship between popularity and # of followers?")

#ggsave("plot1.png", width = 15, height = 11)


# Plot 2

tracks %>%
  pivot_longer(cols = c(danceability:tempo)) %>% 
  filter(value > 0.01) %>%
  mutate(track_name = reorder_within(track_name, value, name)) %>%
  ggplot(aes(value, track_name, fill = album_name)) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~name, scales = "free") +
  labs(x = "",
       y = "",
       fill = "album name",
       title = "Track names and key information") +
  theme(strip.text = element_text(size = 15),
        plot.title = element_text(size = 18))

#ggsave("plot2.png", width = 17, height = 14)

# Plot 3

lyrics %>%
  group_by(album_name) %>%
  mutate(row_id = row_number()) %>% 
  ungroup() %>%
  unnest_tokens(word, line) %>%
  count(album_name, word) %>%
  anti_join(stop_words) %>%
  bind_tf_idf(word, album_name, n) %>%
  group_by(album_name) %>%
  slice_max(tf_idf, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, album_name)) %>%
  ggplot(aes(tf_idf, word, name = word, fill = n)) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~album_name, scales = "free") +
  theme(
        strip.text = element_text(size = 15),
        plot.title = element_text(size = 18)) +
  labs(x = "TF-IDF",
       y = "lyrics word",
       fill = "word count",
       title = "Top 10 lyrics words with the largest TF-IDF values") +
  scale_fill_gradient2(high = "green",
                       low = "red",
                       mid = "pink",
                       midpoint = 20) 
  
  
#ggsave("plot3.png", width = 11, height = 10)












