library(tidyverse)
investment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/investment.csv')
chain_investment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/chain_investment.csv')
ipd <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/ipd.csv')


# inner join investement and chain_investment by category and year
ic <- inner_join(investment, chain_investment, by = c("category","group_num", "meta_cat","year"))


# chained investment

ic %>% group_by(meta_cat, year) %>%
  summarize(avg = mean(gross_inv_chain), sd = sd(gross_inv_chain)) %>%
  ggplot(aes(year)) +
  geom_line(aes(y = avg, color = "Mean")) +
  geom_line(aes(y = sd, color = "Standard Deviation"), alpha = 0.8) +
  geom_point(aes(y = avg, color = "Mean")) +
  geom_point(aes(y = sd, color = "Standard Deviation"), shape = 5) +
  facet_wrap(~meta_cat, scales = "free") +
  scale_color_manual(name = "", values = c("Mean" = "blue", "Standard Deviation" = "orange")) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 16, face = "bold"),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 18)
  ) +
  scale_x_continuous(breaks = seq(1950, 2020, 15)) +
  labs(x = "year", y = "Chained Gross Investment (in millions of USD)", title = "Category-wise Chained Average Investment & Standard Deviation")

#ggsave("Chained Gross Investment.png", width = 20, height = 10)



# unchained investment
ic %>% group_by(meta_cat, year) %>%
  summarize(avg = mean(gross_inv), sd = sd(gross_inv)) %>%
  ggplot(aes(year)) +
  geom_line(aes(y = avg, color = "Mean")) +
  geom_line(aes(y = sd, color = "Standard Deviation"), alpha = 0.8) +
  geom_point(aes(y = avg, color = "Mean")) +
  geom_point(aes(y = sd, color = "Standard Deviation"), shape = 5) +
  facet_wrap(~meta_cat, scales = "free") +
  scale_color_manual(name = "", values = c("Mean" = "blue", "Standard Deviation" = "orange")) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 16, face = "bold"),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 18)
  ) +
  scale_x_continuous(breaks = seq(1950, 2020, 15)) +
  labs(x = "year", y = "Gross Investment (in millions of USD)", title = "Category-wise Average Investment & Standard Deviation")

#ggsave("Gross Investment.png", width = 20, height = 10)

# ipd

ipd %>%
  group_by(meta_cat, year) %>%
  summarize(avg = mean(gross_inv_ipd, na.rm = TRUE)) %>%
  ggplot(aes(year, avg, color = meta_cat)) +
  geom_line() +
  geom_point() +
  facet_wrap(~meta_cat, scale = "free") +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 16, face = "bold"),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 15),
    plot.title = element_text(size = 18)
  ) +
  labs(y = "Implicit Price Deflators", title = "Category-wise Average Implicit Price Deflators")

#ggsave("Category-wise Average Implicit Price Deflators.png", width = 20, height = 10)
