library(tidyverse)
library(lubridate)
library(geofacet)
library(tidytext)
theme_set(theme_light())

airmen <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv') %>%
  mutate(state = str_to_upper(state)) %>%
  filter(fct_lump(rank_at_graduation, n = 3) != "Other") %>%
  mutate(pilot_type = fct_recode(pilot_type, "Liaison pilot" = "Liason pilot"),
         pilot_type = str_to_title(pilot_type))

# plot 1

airmen %>%
  count(state, pilot_type, rank_at_graduation, sort = T) %>%
  filter(!is.na(state)) %>%
  mutate(pilot_type = reorder_within(pilot_type, n, state, sum)) %>%
  ggplot(aes(n, pilot_type, fill = rank_at_graduation)) +
  geom_col() +
  scale_y_reordered() +
  facet_geo(~ state, scales = "free_y") +
  theme(strip.text = element_text(size = 15),
        axis.title = element_text(size = 13),
        plot.title = element_text(size = 18)) +
  labs(x = "# of graduates",
       y = "pilot type",
       fill = "graduation rank",
       title = "State-wise # of Graduates")

#ggsave("plot1.png", width = 18, height = 11)

# plot 2

airmen %>%
  mutate(graduated_from = str_remove(graduated_from, ",.+$")) %>%
  ggplot(aes(graduation_date, fill = graduated_from)) +
  geom_histogram(alpha = 0.8) +
  labs(fill = "graduated from",
       x = "graduation date",
       y = "# of graduates",
       title = "How were the Graduation Dates Distributed?") +
  theme(strip.text = element_text(size = 15),
        axis.title = element_text(size = 13),
        plot.title = element_text(size = 18))


#ggsave("plot2.png", width = 15, height = 11)

# plot 3

airmen %>%
  count(military_hometown_of_record, state, sort = T) %>%
  head(10) %>%
  mutate(military_hometown_of_record = fct_reorder(military_hometown_of_record, n),
         state = fct_reorder(state, -n)) %>%
  ggplot(aes(n, military_hometown_of_record, fill = state)) +
  geom_col() +
  labs(x = "# of graduates",
       y = NULL,
       title = "Top 10 Cities that Produced the Most Graduates") +
  theme(strip.text = element_text(size = 15),
        axis.title = element_text(size = 13),
        plot.title = element_text(size = 18))


#ggsave("plot3.png", width = 13, height = 11)

## Plot 4

airmen_credits <- airmen %>%
  rename(credits = aerial_victory_credits) %>%
  filter(credits > 0) %>%
  select(name, graduation_date, rank_at_graduation, pilot_type, credits) %>%
  separate_rows(credits, sep = "; |and ") %>%
  mutate(credits = str_remove_all(credits, "Downed |\\son")) %>%
  separate(credits, into = c("num_of_planes_shot", "shot_plane_type", "shot_date"), sep = "\\s", extra = "merge") %>%
  mutate(shot_date = na_if(shot_date, "")) %>%
  fill(shot_date, .direction = "up") %>%
  filter(str_detect(shot_plane_type, "[:alpha:]")) %>%
  mutate(shot_plane_type = fct_lump(str_to_upper(str_remove(shot_plane_type, "s$")), n = 2),
         shot_date = mdy(shot_date),
         diff_time = difftime(shot_date, graduation_date),
         num_of_planes_shot = as.numeric(num_of_planes_shot))

airmen_credits %>%
  ggplot(aes(diff_time, fill = shot_plane_type)) +
  geom_histogram(binwidth = 40, alpha = 0.8) +
  scale_x_continuous(breaks = c(150, 200, 300, 400, 500, 600, 700, 1000)) +
  theme(panel.grid = element_blank()) +
  labs(x = "days between graduation and getting plane down",
       y = "# of planes shot",
       fill = "shot plane type",
       title = "How Long did Graduates Wait to Down the Planes?") +
  theme(strip.text = element_text(size = 15),
        axis.title = element_text(size = 13),
        plot.title = element_text(size = 18))

#ggsave("plot4.png", width = 13, height = 11)


