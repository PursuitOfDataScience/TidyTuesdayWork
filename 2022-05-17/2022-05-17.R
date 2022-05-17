library(tidyverse)
library(ggDoubleHeat)
library(tidymodels)
library(themis)
theme_set(theme_bw())

eurovision <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv') %>%
  select(-contains("url"), -country_emoji, -rank_ordinal) %>%
  mutate(guest = if_else(host_country == artist_country, "foreigner artist", "home artist")) 
  


# Plot 1
 
eurovision %>%
  group_by(host_country, guest) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(host_country = fct_reorder(host_country, n, sum)) %>%
  ggplot(aes(n, host_country, fill = guest)) +
  geom_col(alpha = 0.7) +
  scale_fill_manual(values = c("green", "purple")) +
  theme(panel.grid = element_blank()) +
  labs(x = "# of artists",
       y = "host country",
       fill = "",
       title = "How many artists are home and foreigner artists per host country?") 

#ggsave("plot1.png", width = 10, height = 7)

# Plot 2

eurovision %>%
  group_by(host_country, guest, winner) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(host_country, guest) %>%
  mutate(total_artists = sum(n),
         pct = n/total_artists) %>%
  ungroup() %>%
  pivot_wider(names_from = winner,
              values_from = pct,
              values_fill = 0) %>%
  rename(Winner = `TRUE`,
         Loser = `FALSE`) %>%
  ggplot(aes(guest, host_country)) +
  geom_heat_grid(outside = Winner,
                 inside = Loser,
                 labels = scales::percent) +
  labs(x = NULL,
       y = "host country",
       title = "How likely is the winning percentage for foreinger and home artists per country?")

#ggsave("plot2.png", width = 10, height = 7)

# Built the LASSO model

euro <- eurovision %>%
  select(year, host_country, guest, rank, winner, total_points) %>%
  mutate(winner = if_else(winner, "winner", "loser"),
         across(where(is.character), factor))

set.seed(2022)
euro_spl <- euro %>%
  initial_split(strata = "winner")
euro_train <- training(euro_spl)
euro_test <- testing(euro_spl)



set.seed(2022)
euro_folds <- vfold_cv(euro_train, strata = "winner")

euro_rec <- recipe(winner ~ ., data = euro_train) %>%
  step_impute_knn(total_points) %>%
  step_impute_bag(rank) %>%
  step_downsample(winner) %>%
  step_other(host_country) %>%
  step_dummy(all_nominal_predictors())

euro_spec <- logistic_reg(penalty = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

euro_wf <- workflow() %>%
  add_model(euro_spec) %>%
  add_recipe(euro_rec)

# 10-fold CV

euro_res <- euro_wf %>%
  tune_grid(
    euro_folds,
    grid = crossing(penalty = 10 ^ seq(-7, -0.5, 0.5))
  )

# Plot 3

autoplot(euro_res) +
  theme(panel.grid = element_blank()) +
  ggtitle("LASSO Classification (10-fold CV)")

#ggsave("plot3.png", height = 7, width = 10)

# Plot 4

euro_last_fit <- euro_wf %>%
  finalize_workflow(select_best(euro_res, "roc_auc")) %>%
  last_fit(euro_spl)

euro_last_fit %>%
  collect_predictions() %>%
  roc_curve(winner, .pred_loser) %>%
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_path(size = 2, color = "green") +
  geom_abline(size = 2, lty = 2, color = "red", alpha = 0.5) +
  theme(panel.grid = element_blank()) +
  labs(title = "ROC Curve for Predicting Eurovision Winner") 

#ggsave("plot4.png", height = 7, width = 10)





