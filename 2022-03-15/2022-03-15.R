library(tidyverse)
library(lubridate)
theme_set(theme_bw())

bioc <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/bioc.csv') %>%
  mutate(date = str_remove(date, "\\s.+$"),
         date = ymd(date)) %>%
  distinct() %>%
  filter(date > 2000)

cran <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv') %>%
  mutate(date = str_remove(date, "\\s.+$"),
         date = ymd(date),
         version = str_replace(version, "-", "\\."))

# Plot 1

bioc %>%
  add_count(package) %>%
  filter(n > 200) %>%
  group_by(package) %>%
  slice_max(date, n = 1, with_ties = F) %>%
  ungroup() %>%
  ggplot(aes(rnw, rmd, color = package)) +
  geom_jitter(alpha = 0.3) +
  geom_text(aes(label = package),
            check_overlap = T,
            vjust = -1,
            hjust = 0) +
  scale_x_continuous(breaks = seq(1,10)) +
  scale_y_continuous(breaks = seq(1,20)) +
  labs(x = "RNW",
       y = "RMD",
       title = "RMD & RMD") +
  theme(panel.grid = element_blank(),
        legend.position = "none")

#ggsave("plot1.png", width = 10, height = 6)

# Plot 2

cran %>%
  add_count(package, name = "update_times") %>%
  filter(update_times > 100) %>% 
  group_by(package, update_times) %>%
  slice_max(date, n = 1) %>%
  ungroup() %>%
  mutate(package = paste0(package, "(rnw:", rnw, ", rmd:", rmd, ")")) %>%
  mutate(package = fct_reorder(package, update_times)) %>%
  ggplot(aes(update_times, package, fill = package)) +
  geom_col(show.legend = F) +
  labs(x = "# of updates",
       y = NULL,
       title = "The R Packages with Most # of Updates")

#ggsave("plot2.png", width = 10, height = 6)

# Plot 3

bioc %>%
  arrange(date) %>%
  group_by(package) %>%
  mutate(n = n(),
         lag_date = lag(date),
         update_gap = date - lag_date) %>%
  filter(!is.na(update_gap),
         n > 400,
         update_gap < 400) %>%
  ungroup() %>%
  mutate(update_gap = as.numeric(update_gap),
         package = fct_reorder(package, update_gap)) %>%
  ggplot(aes(update_gap, package, fill = package)) +
  geom_boxplot(outlier.alpha = 0.3,
               show.legend = F) +
  scale_x_continuous(breaks = c(1, 10, 50, 100, 150, 200)) +
  labs(x = "update days",
       y = NULL,
       title = "Updates for R Packages") 

#ggsave("plot3.png", width = 10, height = 6)


