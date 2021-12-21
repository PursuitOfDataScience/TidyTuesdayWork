library(tidyverse)
library(tidytext)
theme_set(theme_bw())

starbucks <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv") %>%
  mutate(fiber_g = as.numeric(fiber_g),
         trans_fat_g = as.numeric(trans_fat_g),
         size = factor(size, levels = c("short", "tall", "grande", "venti")))

## Plot 1

starbucks %>%
  mutate(product_name = fct_lump(product_name, n = 16)) %>%
  filter(product_name != "Other") %>%
  pivot_longer(cols = c(milk:caffeine_mg)) %>%
  mutate(name = str_replace_all(name, "_", " "),
         name = str_replace(name, "serv size m l", "serve size (ml)"),
         name = str_replace(name, "mg", "(mg)"),
         name = str_replace(name, " g", " (g)")) %>%
  mutate(name = reorder_within(name, value, product_name)) %>%
  ggplot(aes(value, name, fill = size)) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~product_name, scales = "free") +
  theme(strip.text = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 18)) +
  labs(x = NULL,
       y = "nutrition fact",
       title = "16 Most Popular Coffee Products & Their Ingredients (units included)")

#ggsave("plot1.png", width = 20, height = 10)

## Plot 2

starbucks %>%
  filter(str_detect(product_name, "brewed coffee|Brewed Coffee"),
         !is.na(size)) %>% 
  mutate(product_name = str_to_title(product_name)) %>%
  ggplot(aes(size, product_name, fill = caffeine_mg)) +
  geom_tile() +
  scale_fill_gradient2(high = "red",
                       low = "green",
                       mid = "pink",
                       midpoint = 200) +
  theme(panel.grid = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  labs(x = NULL,
       y = NULL,
       fill = "caffeine (mg)",
       title = "How much does brewed coffee contain caffeine?") 

#ggsave("plot2.png", width = 11, height = 8)

## Plot 3

starbucks %>%
  filter(caffeine_mg > 0, sugar_g > 0, !is.na(size)) %>%
  ggplot(aes(sugar_g, caffeine_mg, color = product_name)) +
  geom_point() +
  geom_text(aes(label = product_name), vjust = 1, hjust = 1, check_overlap = T) +
  labs(x = "sugar (g)",
       y = "caffeine (mg)",
       title = "More sugar, more caffeine?") +
  theme(legend.position = "none",
        strip.text = element_text(size = 15),
        plot.title = element_text(size = 18)) +
  facet_wrap(~size) 

#ggsave("plot3.png", width = 13, height = 8)

## Plot 4

starbucks %>%
  filter(!is.na(size)) %>%
  mutate(product_name = str_to_lower(product_name),
         cold_or_hot = if_else(str_detect(product_name, "cold|iced"), "Cold or Iced Coffee", "Hot Coffee")) %>% 
  select(product_name, size, calories, sugar_g, caffeine_mg, cold_or_hot) %>%
  pivot_longer(cols = c(calories:caffeine_mg)) %>%
  mutate(name = str_replace(name, "_", " ")) %>%
  ggplot(aes(value, cold_or_hot, fill = name)) +
  geom_boxplot() +
  facet_wrap(~size, ncol = 2) +
  labs(x = NULL,
       y = NULL,
       fill = "nutrition fact",
       title = "Cold Coffee V.S. Hot Coffee") +
  theme(strip.text = element_text(size = 15),
        plot.title = element_text(size = 18))

#ggsave("plot4.png", width = 13, height = 8)  







