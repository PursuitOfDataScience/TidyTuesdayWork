library(tidyverse)
library(lubridate)
theme_set(theme_light())

big_tech_stock_prices <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

# combine both tibbles

big_tech <- big_tech_stock_prices |>
  left_join(big_tech_companies, by = "stock_symbol")


# plot 1

big_tech |>
  ggplot(aes(date, volume, color = company)) +
  geom_line(show.legend = F, alpha = 0.7) +
  scale_y_log10() +
  facet_wrap(~company) +
  labs(x = "",
       title = "Big Tech Daily Stock Trading Volume")

#ggsave("plot1.png", height = 7, width = 12)

# plot 2

big_tech |>
  ggplot(aes(date, close, color = company)) +
  geom_line() +
  geom_ribbon(aes(ymin = low,
                  ymax = high),
              alpha = 0.6) +
  facet_wrap(~company, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "none") +
  labs(x = NULL,
       title = "Big Tech Daily Closing Price")

#ggsave("plot2.png", height = 7, width = 12)


# plot 3

big_tech |>
  mutate(diff = high - low) |>
  ggplot(aes(diff, volume, color = stock_symbol)) +
  geom_point(alpha = 0.1) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  scale_y_log10() +
  scale_x_log10() +
  labs(x = "daily high - daily low",
       y = "daily volume",
       color = "stock symbol",
       title = "Daily Trading Volue and Daily Stock Price Range")


#ggsave("plot3.png", height = 7, width = 12)

# plot 4

big_tech |>
  mutate(year = year(date),
         month = month(date),
         day = day(date)) |>
  group_by(year, month, company) |>
  summarize(open = first(open),
            close = last(close)) |>
  ungroup() |>
  mutate(year_month = make_date(year, month)) |>
  ggplot(aes(x = year_month, color = factor(year_month))) +
  geom_errorbar(aes(ymin = open,
                    ymax = close)) +
  facet_wrap(~company, scales = "free_y") +
  theme(legend.position = "none") +
  labs(x = "",
       y = "price",
       title = "Monthly Starting and Ending Stock Price (Error Bar)")

#ggsave("plot4.png", height = 7, width = 12)







