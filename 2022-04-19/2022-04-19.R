library(tidyverse)
library(lubridate)
library(patchwork)
library(tidytext)
library(ggraph)
theme_set(theme_bw())

big_dave <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/big_dave.csv')
times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv')

crossword_line <- function(df){
  df %>%
    count(puzzle_date) %>%
    group_by(year = year(puzzle_date),
             month = month(puzzle_date)) %>%
    filter(!is.na(year)) %>%
    summarize(total_puzzles = sum(n),
              n = n()) %>%
    ungroup() %>%
    
    mutate(year_month = make_date(year, month)) %>%
    ggplot(aes(year_month, total_puzzles)) +
    geom_line() +
    geom_point(aes(size = n, color = factor(month))) +
    geom_vline(xintercept = as.Date("2020-03-01"),
               size = 1,
               lty = 2,
               color = "red") +
    scale_x_date(date_breaks = "2 years",
                 date_labels = "%Y") +
    scale_size_continuous(range = c(1,4)) +
    scale_color_discrete(guide = "none") +
    theme(panel.grid = element_blank()) +
    labs(x = NULL,
         y = "# of puzzles",
         size = "# of days puzzles\noffered in a month",
         subtitle = "Red dashed line reprsents COVID outbreak") 
}  
  

# Plot 1

p1 <- big_dave %>%
  crossword_line(.) +
  ggtitle("Big Dave # of Monthly Puzzles") 

p2 <- times %>%
  crossword_line(.) +
  ggtitle("Times # of Monthly Puzzles") 


p1 / p2

#ggsave("plot1.png", width = 10, height = 8)

# Plot 2

word_answer_tbl <- big_dave %>%
  unnest_tokens(word, clue) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, "[:digit:]")) %>%
  count(word, answer, sort = T) %>%
  filter(n > 5)

word_answer_tbl  %>%
  head(500) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(width = n),
                 alpha = 0.5) +
  geom_node_point() +
  geom_node_text(aes(label = name, color = name), hjust = 0.5, vjust = 1, check_overlap = T, size = 4) +
  scale_edge_width_continuous(range = c(1,3)) +
  theme_void() +
  guides(color = "none") +
  labs(edge_width = "clue & answer word count",
       title = "How are clue words and ANSWER related?") +
  theme(plot.title = element_text(size = 18))

#ggsave("plot2.png", width = 10, height = 8)

# Plot 3

big_dave %>%
  filter(!is.na(answer),
         !is.na(definition)) %>%
  count(answer, sort = T) %>%
  head(20) %>%
  mutate(source = "Big Dave") %>%
  bind_rows(
    times %>%
      filter(!is.na(answer),
             !is.na(definition)) %>%
      count(answer, sort = T) %>%
      head(20) %>%
      mutate(source = "Times")
  ) %>%
  mutate(answer = fct_reorder(answer, n, sum)) %>%
  ggplot(aes(n, answer, fill = source)) +
  geom_col() +
  labs(x = "# of words",
       y = "puzzle answer",
       fill = "puzzle source",
       title = "Top 20 Answer Words from Both Sources")

#ggsave("plot3.png", width = 10, height = 8)

