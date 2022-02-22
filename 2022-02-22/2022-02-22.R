library(tidyverse)
library(tidytext)
library(scales)
theme_set(theme_light())

freedom <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv') %>%
  janitor::clean_names() %>%
  mutate(status = fct_recode(status, 
                             "Not Free" = "NF",
                             "Partially Free" = "PF",
                             "Free" = "F"),
         status = factor(status, levels = c("Free",
                                            "Partially Free",
                                            "Not Free"))) 

# Plot 1

freedom %>%
  group_by(year, status, region_name) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(status = fct_reorder(status, n, sum)) %>%
  ggplot(aes(year, n, color = status)) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~region_name) +
  theme(legend.position = c(0.8, 0.2),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        plot.title = element_text(size = 18),
        strip.text = element_text(size = 15),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12)) +
  labs(x = NULL,
       y = "# of countries",
       title = "The Raw Count of Freedom Status across All Continents (1995 - 2020)")

#ggsave("plot1.png", width = 15, height = 11)

# Plot 2

freedom %>%
  group_by(year, status, region_name) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(year, region_name) %>%
  mutate(total_countries = sum(n),
         ratio = n/total_countries) %>%
  ungroup() %>%
  mutate(status = fct_reorder(status, ratio, sum)) %>%
  ggplot(aes(year, ratio, fill = status)) +
  geom_col() +
  facet_wrap(~region_name) +
  theme(legend.position = c(0.8, 0.2),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        plot.title = element_text(size = 18),
        strip.text = element_text(size = 15),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL,
       y = "percentage of countries",
       title = "The Percentage of Freedom Status across All Continents (1995 - 2020)")

#ggsave("plot2.png", width = 15, height = 11)

# Plot 3

freedom %>%
  mutate(decade = paste("Decade:", 10 * floor(year/10)),
         cl_pr_combined = cl + pr) %>%
  mutate(region_name = reorder_within(region_name, cl_pr_combined, decade)) %>%
  ggplot(aes(cl_pr_combined, region_name, fill = region_name)) +
  geom_boxplot(show.legend = F) +
  scale_y_reordered() +
  facet_wrap(~decade, scales = "free_y") +
  labs(x = "civil liberties + public rights",
       y = NULL,
       title = "Decade-wise Civil Liberties and Public Rights Scores Combined") +
  theme(strip.text = element_text(size = 15),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18))

#ggsave("plot3.png", width = 13, height = 11)

# Plot 4

freedom %>%
  mutate(cl_pr_combined = cl + pr) %>%
  filter(year %in% c(1995, 2020)) %>%
  group_by(country) %>%
  mutate(cl_pr_combined_lag = lag(cl_pr_combined),
         progress = cl_pr_combined - cl_pr_combined_lag) %>% 
  ungroup() %>%
  group_by(progress > 0) %>%
  slice_max(abs(progress), n = 6) %>% 
  ungroup() %>%
  mutate(country = fct_reorder(country, progress)) %>%
  ggplot(aes(progress, country, fill = progress > 0)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "Score Difference between 1995 and 2020",
       y = NULL,
       title = "Top 6 Countries Changing the Most on Combined Scores",
       subtitle = "Combined scores are defined by adding the score of civil liberties and of political rights")
  
#ggsave("plot4.png", width = 15, height = 11)

  
  

