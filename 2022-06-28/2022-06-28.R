library(tidyverse)
library(lubridate)
library(ggDoubleHeat)
theme_set(theme_bw())

paygap <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv') %>%
  mutate(date_submitted = ymd(str_remove(date_submitted, "\\s.+$")),
         employer_size = fct_reorder(employer_size, parse_number(employer_size)),
         employer_size = fct_relevel(employer_size, "Less than 250")) 

# Plot 1
paygap %>%
  pivot_longer(diff_mean_hourly_percent:female_top_quartile) %>%
  mutate(name = str_replace_all(name, "_", " ")) %>%
  filter(value > 0,
         value < 1000) %>%
  ggplot(aes(value, fill = employer_size)) +
  geom_histogram(alpha = 0.6) +
  facet_wrap(~name) +
  scale_x_log10(labels = scales::percent) +
  labs(x = NULL,
       fill = "employer size",
       title = "UK Pay Gap Statistics Overview")

#ggsave("plot1.png", height = 8, width = 12)

# Plot 2

paygap %>%
  transmute(diff_mean_hourly_percent, diff_mean_bonus_percent, 
            year = year(date_submitted), employer_name) %>%
  na.omit() %>%
  group_by(year, employer_name) %>%
  summarize(across(1:2, mean),
            n = n()) %>%
  filter(n > 3) %>%
  ggplot(aes(year, employer_name)) +
  geom_heat_grid(outside = diff_mean_hourly_percent,
                 outside_name = "mean hourly difference",
                 inside = diff_mean_bonus_percent,
                 inside_name = "mean bonus difference",
                 labels = scales::percent) +
  theme_heat() +
  labs(title = "Selected Employers and Hourly, Bonus Differences")

#ggsave("plot2.png", height = 9, width = 12)

# Plot 3

paygap %>%
  group_by(year = year(date_submitted), employer_size) %>%
  summarize(n_employer = n_distinct(employer_name)) %>%
  ungroup() %>%
  filter(employer_size != "Not Provided") %>%
  ggplot(aes(year, n_employer, fill = employer_size)) +
  geom_area(show.legend = F, alpha = 0.7) +
  geom_vline(xintercept = 2020, lty = 2, color = "red", size = 1) +
  facet_wrap(~employer_size) +
  scale_x_continuous(breaks = seq(2017, 2022, 2)) +
  labs(x = NULL,
       y = "# of distinct employers",
       title = "# of Distinct Employers Faceted by Size Providing Paygap Info Per Year",
       subtitle = "Red dashed line refers to 2020")

#ggsave("plot3.png", height = 8, width = 10)

# Plot 4

paygap %>%
  group_by(submit_late = submitted_after_the_deadline,
           employer_size) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(submit_late) %>%
  mutate(total_submit_late = sum(n),
         ratio = n/total_submit_late) %>%
  ungroup() %>%
  mutate(submit_late = paste("Late Submission:", submit_late)) %>%
  ggplot(aes(ratio, employer_size, fill = submit_late)) +
  geom_col(show.legend = F) +
  facet_wrap(~submit_late, ncol = 1) +
  scale_fill_manual(values = c("lightgreen", "magenta")) +
  scale_x_continuous(labels = scales::percent) +
  labs(y = "employer size",
       title = "What Employers Tended to Submit Paygap Info Late?",
       subtitle = "Ratio is computed by using individual count divides total count within each submission")

#ggsave("plot4.png", height = 8, width = 12)

 


