library(tidyverse)
library(geofacet)
library(scales)
library(tidytext)
theme_set(theme_bw())

sports <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv') %>%
  rename(city = city_txt,
         state = state_cd) %>%
  rename_with(~str_remove_all(.x, "^ef_|_text$")) %>%
  filter(!is.na(state))


# plot 1

sports %>%
  pivot_longer(rev_men:total_exp_menwomen, names_to = "cost_category", values_to = "dollar") %>%
  group_by(state, sports, year) %>%
  filter(cost_category %in% c("total_exp_menwomen")) %>%
  summarize(avg_dollar = mean(dollar, na.rm = T)) %>% 
  ungroup() %>%
  group_by(state, year) %>%
  slice_max(avg_dollar, n = 5) %>%
  ungroup() %>%
  mutate(sports = reorder_within(sports, avg_dollar, state)) %>%
  ggplot(aes(avg_dollar/1000000, sports, fill = factor(year))) +
  geom_col() +
  scale_y_reordered() +
  facet_geo(~state, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous(labels = dollar, n.breaks = 3) +
  labs(x = "average total expenditure (in millions)",
       y = NULL,
       fill = "year",
       title = "Yearly Top 5 Sports per State on Average Total Expenditure")

#ggsave("plot1.png", width = 20, height = 12)


# plot 2

sports %>%
  mutate(per_male_exp = exp_men/partic_men,
         per_female_exp = exp_women/partic_women) %>%
  group_by(year, state) %>%
  summarize(across(per_male_exp:per_female_exp, mean, na.rm = T)) %>%
  ungroup() %>%
  rename(Male = per_male_exp,
         Female = per_female_exp) %>%
  pivot_longer(3:4, names_to = "sex", values_to = "exp") %>%
  ggplot(aes(year, exp, fill = sex)) +
  geom_area(alpha = 0.5) +
  scale_x_continuous(breaks = seq(2015, 2020, 2)) +
  scale_y_continuous(labels = dollar) +
  facet_geo(~state) +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid = element_blank(),
        strip.text = element_text(size = 13),
        plot.title = element_text(size = 18)) +
  labs(x = NULL,
       y = "average expenditure per person",
       fill = NULL,
       title = "Average Expenditure per State per Year per Athlete")

#ggsave("plot2.png", width = 13, height = 10)

# plot 3

sports %>%
  mutate(profit = total_rev_menwomen - total_exp_menwomen,
         sports = fct_lump(sports, n = 10),
         sports = fct_reorder(sports, -total_exp_menwomen, sum)) %>%
  ggplot(aes(total_exp_menwomen, total_rev_menwomen, color = sports)) +
  geom_point(alpha = 0.2) +
  geom_abline(color = "grey50", lty = 2, size = 1.5) +
  geom_text(aes(label = institution_name), 
            vjust = 1,
            alpha = 0.6,
            hjust = 1,
            check_overlap = T) +
  scale_x_log10(labels = dollar) +
  scale_y_log10(labels = dollar) +
  labs(y = "total revenue",
       x = "total expenditure",
       title = "Do schools make profits from sports?") +
  theme(panel.grid = element_blank())

#ggsave("plot3.png", width = 13, height = 10)

# plot 4

sports %>%
  filter(institution_name != "Greensboro College",
         year != 2015) %>%
  group_by(institution_name, year, state) %>%
  summarize(total_spending = sum(total_exp_menwomen, na.rm = T),
            total_making = sum(total_rev_menwomen, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  mutate(profit_pct = (total_making - total_spending)/total_spending,
         institution_name = paste0(institution_name, "(", state, ")")) %>%
  group_by(year) %>%
  slice_max(profit_pct, n = 10) %>%
  ungroup() %>%
  mutate(institution_name = reorder_within(institution_name, profit_pct, year)) %>%
  ggplot(aes(profit_pct, institution_name, fill = state)) +
  geom_col(show.legend = F) +
  scale_x_continuous(labels = percent) +
  scale_y_reordered() +
  facet_wrap(~year, scales = "free_y") +
  labs(x = "profit",
       y = NULL,
       title = "Top 5 Most Lucrative Schools from Sports") +
  theme(strip.text = element_text(size = 13),
        plot.title = element_text(size = 16)) 

#ggsave("plot4.png", width = 15, height = 10)  






















