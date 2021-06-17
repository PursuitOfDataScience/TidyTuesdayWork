library(tidyverse)
tweets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-15/tweets.csv')

#initial data analysis --------
dim(tweets)
colSums(is.na(tweets))
str(tweets)

tweets %>% drop_na(location) %>% group_by(location) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

followers <- tweets %>% drop_na() %>%
  group_by(username) %>%
  summarize(followers = followers, `total like count` = sum(like_count)) %>%
  ungroup() %>%
  distinct() %>%
  arrange(desc(followers)) 
  
cor(followers$followers, followers$`total like count`)

library(lubridate)
tweets$datetime <- as.Date(ymd_hms(tweets$datetime))

tweets <- tweets %>% mutate(
  weekofday = weekdays(datetime)
)
tweets %>% group_by(weekofday, location) %>%
  summarize(`tweet count` = n()) %>%
  arrange(desc(`tweet count`))

tweets$weekofday <- factor(tweets$weekofday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
                                                        "Sunday", NA))
tweets %>% group_by(weekofday) %>%
  summarize(`tweet count` = n()) %>%
  arrange(desc(`tweet count`)) %>%
  ggplot(aes(weekofday, `tweet count`, fill = weekofday)) +
  geom_bar(stat = "identity")+
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.title = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 10),
    legend.position = "none",
    axis.title.y = element_blank(),
    plot.title = element_text(size = 16)
  ) +
  coord_flip() +
  ggtitle("Tweets Distribution Across The Week")

#ggsave("Tweets Distribution Across The Week.png", width = 20, height = 10)


tweets %>% group_by(username) %>%
  summarize(`tweet count` = n(), followers, `total like counts` = sum(like_count)) %>%
  arrange(desc(`tweet count`)) %>%
  distinct() %>%
  head(10) %>%
  ggplot(aes(username, `tweet count`, fill = followers)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradientn(colors = rainbow(5))+
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.title = element_text(size = 15),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    plot.title = element_text(size = 16)
  ) +
  ggtitle("Top 10 Tweet Users On #DuBoisChallenge")

#ggsave("Top 10 Tweet Users On #DuBoisChallenge.png", width = 20, height = 10)


library(tidytext)

tweets%>% select(content) %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n >= 56) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = word)) +
  geom_col() +
  labs(y = NULL) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.title = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    legend.position = "none",
    plot.title = element_text(size = 16)
  ) +
  labs(x = "word counts", y = "word", title = "Top 10 Most Frequently Words In Tweets (stop words excluded)")

get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tweets %>% select(content)%>% 
  mutate(linenumber = row_number())

tweets%>% select(content) %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words) %>%
  #count(word, sort = TRUE) %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN") %>%
  ggplot(aes(index, sentiment, fill = ifelse(sentiment < 0, "red", "blue"))) +
    geom_col(show.legend = FALSE, width = 2) +
  labs(x = "tweet", title = "Tweet Sentiment Distribution (AFINN)") +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.title = element_text(size = 15),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    plot.title = element_text(size = 16)
  )


afinn <- tweets%>% select(content) %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words) %>%
  #count(word, sort = TRUE) %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(linenumber) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN") 

bing_and_nrc  <- bind_rows(
tweets%>% select(content) %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(method = "Bing et al."),
tweets%>% select(content) %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("nrc")) %>% 
               filter(sentiment %in% c("positive", 
                                       "negative"))%>%
               mutate(method = "NRC")) %>%
  count(method, linenumber, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) 

bing_and_nrc <- bing_and_nrc %>% 
  mutate(sentiment = positive - negative)

bing_and_nrc <- bing_and_nrc %>% select(linenumber, sentiment, method)


bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(linenumber, sentiment, fill = method)) +
  geom_col(show.legend = FALSE, width = 1) +
  facet_wrap(~method, ncol = 1, scales = "free_y") +
  labs(x = "tweet", title = "Tweets Sentiment Distribution") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 15),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 15),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    plot.title = element_text(size = 16)
  )

#ggsave("Sentiment Analysis.png", width = 20, height = 10)
             