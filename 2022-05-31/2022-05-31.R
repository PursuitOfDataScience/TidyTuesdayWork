library(tidyverse)
library(tidytext)
library(ggDoubleHeat)
theme_set(theme_bw())

poll <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
reputation <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv') %>%
  rename(category = name)


poll_cleaned <- poll %>%
  pivot_wider(names_from = year,
              values_from = rq) %>%
  rename(`2022` = `2022_rq`,
         rank_2022 = `2022_rank`) %>%
  pivot_longer(cols = c(`2022`, `2017` : `2021`),
               names_to = "year",
               values_to = "rq") %>%
  select(-change, -rank, -`NA`) %>%
  mutate(year = as.numeric(year)) %>%
  distinct()

# Plot 1

poll_cleaned %>%
  filter(rank_2022 < 10,
         !is.na(rq)) %>%
  mutate(company = fct_reorder(company, rank_2022)) %>%
  ggplot(aes(year, rq, fill = company)) +
  geom_area(show.legend = F, alpha = 0.8) +
  facet_wrap(~company) +
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = 13),
        plot.title = element_text(size = 15)) +
  labs(x = NULL,
       y = "RQ score",
       title = "Top 10 Companies with Highest RQ Rank in 2022") 

#ggsave("plot1.png", width = 10, height = 8)


# Plot 2

poll %>%
  filter(!is.na(change),
         year == 2021,
         fct_lump(industry, n = 6) != "Other") %>%
  add_count(industry) %>%
  mutate(company = reorder_within(company, change, industry),
         industry = fct_reorder(industry, -n)) %>%
  ggplot(aes(change, company, fill = industry)) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~industry, scale = "free_y") +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        strip.text = element_text(size = 13),
        plot.title = element_text(size = 15)) +
  labs(x = "rank change from 2021 to 2022",
       y = NULL,
       title = "Company Rank Change from 2021 to 2022 per Industry",
       subtitle = "Change in negative means improvement")

#ggsave("plot2.png", width = 12, height = 8)

# Plot 3

reputation %>%
  mutate(industry = fct_lump(industry, 6),
         category = fct_lump(category, w = score, 3),
         industry = fct_reorder(industry, score),
         category = fct_reorder(category, -score)) %>%
  ggplot(aes(score, industry, fill = category, color = category)) +
  geom_boxplot(alpha = 0.5) +
  theme(panel.grid = element_blank()) +
  labs(x = "reputation score",
       y = NULL,
       title = "Categorical Reputation Score per Industry")

#ggsave("plot3.png", width = 10, height = 8)

# Plot 4

reputation %>%
  group_by(industry, category) %>%
  summarize(mean_rank = mean(rank),
            median_rank = median(rank)) %>%
  ungroup() %>%
  mutate(industry = fct_reorder(industry, mean_rank)) %>%
  ggplot(aes(category, industry)) +
  geom_heat_grid(inside = mean_rank,
                 outside = median_rank,
                 inside_name = "mean rank",
                 outside_name = "median rank") +
  theme_heat() +
  labs(x = NULL,
       title = "Mean and Median Rank of Industry per Category",
       subtitle = "Lower rank means better reputation")

#ggsave("plot4.png", width = 10, height = 8)

  


  

