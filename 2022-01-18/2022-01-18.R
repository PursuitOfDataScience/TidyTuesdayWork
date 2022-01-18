library(tidyverse)
library(scales)
library(tidytext)
library(Matrix)
library(glmnet)
library(broom)
library(ggraph)
chocolate <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv") %>%
  mutate(cocoa_percent = as.numeric(str_remove(cocoa_percent, "%"))) %>%
  rename(specific_bean_origin = specific_bean_origin_or_bar_name)

### Plot 1

chocolate %>%
  mutate(company_manufacturer = fct_lump(company_manufacturer, n = 20)) %>%
  filter(company_manufacturer != "Other") %>%
  group_by(company_manufacturer, company_location) %>%
  summarize(avg_cocoa_percent = mean(cocoa_percent)) %>%
  ungroup() %>%
  mutate(company_manufacturer = fct_reorder(company_manufacturer, avg_cocoa_percent)) %>%
  ggplot(aes(avg_cocoa_percent, company_manufacturer, fill = company_location)) +
  geom_col() +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 15)) +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  labs(x = "average cocoa percent",
       y = "chocolate manufacturer",
       fill = "company location",
       title = "The Average Cocoa % of the 20 Most Popular Chocolate Companies")

#ggsave("plot1.png", width = 12, height = 10)


### Plot 2

set.seed(2022)
chocolate %>%
  count(country_of_bean_origin, company_location, sort = T) %>%
  filter(n > 6) %>%
  ggraph(layout = "stress") +
  geom_edge_link(aes(color = n),
                 width = 1,
                 arrow = arrow(length = unit(4, 'mm'),
                               type = "open")) +
  geom_node_point() +
  geom_node_text(aes(label = name), hjust = 1, vjust = 1, check_overlap = T, size = 5) +
  scale_edge_width_continuous(range = c(1,2)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13)) +
  scale_edge_color_gradient2(low = "red",
                             high = "green",
                             mid = "pink",
                             midpoint = 60) + 
  labs(edge_color = "# of times of origin bean location to company location",
       title = "Where were the cocoa beans from and to which country?",
       subtitle = "The arrows point to the company location, starting from the bean origin country")

#ggsave("plot2.png", width = 12, height = 10)

### Plot 3

chocolate %>%
  mutate(specific_bean_origin = str_remove(specific_bean_origin, ",.+")) %>%
  separate(ingredients, into = c("num_of_ingre", "ingre_types"), sep = "-\\s?", convert = T) %>%
  ggplot(aes(num_of_ingre, rating)) +
  geom_jitter(aes(color = factor(num_of_ingre))) +
  geom_text(aes(label = specific_bean_origin), vjust = 1, hjust = 1, check_overlap = T) +
  theme(legend.position = "none",
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 15)) +
  labs(x = "# of ingredients",
       title = "More ingredients, better rating?",
       subtitle = "Text refers to the specific bean origin") +
  scale_x_continuous(breaks = seq(1,6)) +
  ylim(1,5)

#ggsave("plot3.png", width = 15, height = 8)

## Plot 4

word_matrix <- chocolate %>%
  mutate(row_id = row_number()) %>%
  unnest_tokens(word, most_memorable_characteristics) %>%
  anti_join(stop_words) %>%
  add_count(word) %>%
  cast_sparse(row_id, word)
# Lining up rating with row_id
row_id <- as.integer(rownames(word_matrix))
rating <- chocolate$rating[row_id]
cv_glmnet_model <- cv.glmnet(word_matrix, rating)

cv_glmnet_model$glmnet.fit %>% 
  tidy() %>%
  filter(term != "(Intercept)",
         lambda == cv_glmnet_model$lambda.1se) %>%
  mutate(pos_neg = if_else(estimate > 0, "positive", "negative")) %>%
  group_by(pos_neg) %>%
  slice_max(abs(estimate), n = 20) %>%
  ungroup() %>%
  mutate(term = fct_reorder(term, estimate)) %>%
  ggplot(aes(estimate, term, fill = pos_neg)) +
  geom_col() +
  theme(legend.position = "none",
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 15)) +
  xlim(-1.5,0.5) +
  labs(y = "description word",
       x = "LASSO estimate (within 1 SE)",
       title = "Top 20 chocolate description words contributing most to rating",
       subtitle = "LASSO model used for rating prediction")

#ggsave("plot4.png", width = 12, height = 8)







