library(tidyverse)
library(lubridate)
library(ggthemes)
library(tidytext)
theme_set(theme_tufte())

papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')


# left join all tibbles together

joined_df <- papers %>%
  left_join(paper_authors, by = "paper") %>% 
  left_join(paper_programs, by = "paper") %>% 
  left_join(programs, by = "program") %>% 
  left_join(authors, by = "author")%>%
  unite(date, year:month, sep = "-", remove = FALSE) %>%
  mutate(date = ym(date))

# Plot 1

joined_df %>%
  group_by(date, program_category) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(program_category = fct_reorder(program_category, -count, sum)) %>%
  ggplot(aes(date, count, color = program_category)) +
  geom_line(size = 1) +
  scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
  labs(x = NULL, 
       y = "# of papers",
       color = "program category",
       title = "Category-wise # of Papers from 1973 to 2021"
       )

#ggsave("1.png", width = 15, height = 6) 

# Plot 2

joined_df  %>%
  count(name, program_category, sort = T) %>%
  group_by(program_category) %>%
  slice_max(n, n = 15) %>%
  ungroup() %>%
  mutate(name = reorder_within(name, n, program_category)) %>%
  ggplot(aes(n, name, fill = name)) +
  geom_col(show.legend = F) +
  scale_y_reordered() +
  facet_wrap(~program_category, scales = "free_y") +
  theme(
    strip.text = element_text(size = 15, face = "bold"),
    plot.title = element_text(size = 18),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 10)
  ) +
  labs(x = "# of papers",
       y = NULL,
       title = "Top 15 Authors on Paper Publication across Various Cateogries")

#ggsave("2.png", width = 15, height = 6) 

# Words from paper title

# Plot 3
joined_df %>%
  select(title, year, program_category) %>%
  mutate(row = row_number(),
         decade = as.factor(10 * floor(year/10))) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>%
  count(decade, word, program_category, sort = T) %>% 
  group_by(decade) %>%
  slice_max(word, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, program_category, sum)) %>%
  ggplot(aes(n, word, fill = decade)) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~program_category, scales = "free_y") +
  labs(x = "word count",
       title = "Top 10 Paper Title Words from each Decade",
       subtitle = "Faceted by program category") +
  theme(
    strip.text = element_text(size = 15, face = "bold"),
    plot.title = element_text(size = 17),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 12)
  )

#ggsave("3.png", width = 15, height = 6) 

# Plot 4

joined_df %>%
  select(title, year, program_category) %>%
  mutate(row = row_number(),
         decade = as.factor(10 * floor(year/10))) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>%
  count(decade, word, program_category, sort = T) %>% 
  group_by(decade) %>%
  slice_max(word, n = 10) %>%
  ungroup() %>%
  complete(decade, nesting(program_category, word), fill = list(n = 0)) %>% 
  ggplot(aes(decade, word, fill = n)) +
  geom_tile() +
  facet_wrap(~program_category, scales = "free") +
  scale_fill_gradient2(low = "grey",
                       high = "blue",
                       midpoint = 10,
                       mid = "red") +
  theme(
    strip.text = element_text(size = 15, face = "bold")
  ) +
  labs(title = "Decade-wise Top 10 Words of Paper Titles",
       subtitle = "A slew of values are 0 on the heatmaps")

#ggsave("4.png", width = 15, height = 6) 





  
  
  




