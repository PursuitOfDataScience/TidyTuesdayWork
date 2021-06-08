library(tidyverse)
fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')
stocked <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/stocked.csv')

colSums(is.na(fishing))

top_10_fishes <- fishing %>% group_by(species) %>% count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  top_n(10) %>%
  select(species)

fishing %>% select(-comments) %>%
  filter(species %in% top_10_fishes$species) %>%
  group_by(lake, species, year) %>%
  summarize(`total number of fish`=sum(values)) %>%
  arrange(desc(`total number of fish`)) %>%
  ggplot(aes(year,`total number of fish`, fill = species)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~lake, scales = "free") + 
  theme_bw() +
  theme(
    strip.text = element_text(size=15, face = "bold"),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    plot.title = element_text(size = 16)
  )  + 
  ggtitle("10 Most Populated Fishes In The Great Lakes")
#ggsave("10 Most Populated Fishes In The Great Lakes.png", height = 10, width = 20)

names(stocked) <- tolower(names(stocked))
stocked$year <- as.integer(stocked$year)
colSums(is.na(stocked))
dim(stocked)

library(scales)
stocked %>% select(year, lake, species, no_stocked, weight) %>%
  mutate(lake = case_when(
    lake == "MI" ~ "Michigan",
    lake == "SU" ~ "Superior",
    lake == "ON" ~ "Ontario",
    lake == "ER" ~ "Erie",
    lake == "HU" ~ "Huron",
    lake == "SC" ~ "Saint Clair"
  )) %>%
  ggplot(aes(year, no_stocked/1000, color = lake)) +
  geom_point(size = 2.5) +
  facet_wrap(~species, scales = "free") +
  theme_bw() +
  theme(
    strip.text = element_text(size=15, face = "bold"),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    plot.title = element_text(size = 16)
  ) +
  scale_x_continuous(breaks = seq(1950, 2018, 10)) + 
  labs(x = "year", y = "number stocked (thousands)") +
  ggtitle("Time-Wise Fishes Stocked In The Great Lakes")

#ggsave("Time-Wise Fishes Stocked In The Great Lakes.png", height = 10, width = 20)
