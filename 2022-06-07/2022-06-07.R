library(tidyverse)
library(janitor)
library(scales)
theme_set(theme_bw())

# Data loading

pride_aggregates <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_aggregates.csv') %>%
  clean_names()
fortune_aggregates <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/fortune_aggregates.csv') %>%
  clean_names()
static_list <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/static_list.csv') %>%
  clean_names() %>%
  filter(company != "Grand Total")
pride_sponsors <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_sponsors.csv') %>%
  clean_names()
corp_by_politicians <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/corp_by_politician.csv', 
                                na = "#N/A") %>%
  clean_names() %>%
  filter(politician != "Grand Total")
donors <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/donors.csv') %>%
  clean_names()

# Plot 1

static_list %>%
  filter(number_of_politicians_contributed_to > 1) %>%
  mutate(company = paste0(company, "(", number_of_politicians_contributed_to, ")"),
         company = fct_reorder(company, amount_contributed_across_states)) %>%
  ggplot(aes(amount_contributed_across_states, company, fill = pride)) +
  geom_col() +
  facet_wrap(~pride, scales = "free") +
  scale_x_continuous(labels = dollar) +
  labs(x = "total $ contributed to anti-LBGTQ politicians",
       y = NULL,
       fill = "donated to pride?",
       title = "Company Donations",
       subtitle = "# in bracket represents # of politicians supported") +
  theme(panel.grid = element_blank())

#ggsave("plot1.png", height = 10, width = 12)

# Plot 2

static_list %>%
  ggplot(aes(amount_contributed_across_states, number_of_politicians_contributed_to)) +
  geom_point(aes(size = number_of_states_where_contributions_made, color = pride), alpha = 0.3) +
  geom_text(aes(label = company), vjust = 1, hjust = 0, check_overlap = T) +
  scale_x_log10(labels = dollar) +
  theme(panel.grid = element_blank()) +
  labs(x = "total $ to politician",
       y = "# of politician",
       fill = "donated to pride",
       size = "# of donated states",
       title = "Total $ VS # of Politician") 

#ggsave("plot2.png", height = 7, width = 12)

# Plot 3

corp_by_politicians %>%
  mutate(title = fct_lump(title, n = 3),
         title = fct_reorder(title, sum_of_amount)) %>%
  ggplot(aes(sum_of_amount, title, fill = title)) +
  geom_violin(show.legend = F, alpha = 0.5) +
  geom_point(alpha = 0.3) +
  scale_x_log10(labels = dollar) +
  theme(legend.position = "none",
        panel.grid = element_blank()) +
  labs(x = "total $ politician received",
       y = "",
       title = "Total $ Received by Politicians")

#ggsave("plot3.png", height = 7, width = 10)

# Plot 4

pride_sponsors %>%
  distinct(company, pride_event_sponsored, sponsorship_amount_where_available) %>%
  count(pride_event_sponsored, sponsorship_amount_where_available, sort = T) %>%
  na.omit() %>%
  mutate(sponsorship_amount_where_available = fct_reorder(sponsorship_amount_where_available, n, sum)) %>%
  ggplot(aes(n, sponsorship_amount_where_available, fill = pride_event_sponsored)) +
  geom_col() +
  labs(x = "# of events",
       y = "sponsorship amount level",
       fill = "pride event sponsor",
       title = "Pride Sponsors") +
  theme(panel.grid = element_blank())

#ggsave("plot4.png", height = 10, width = 8)

















