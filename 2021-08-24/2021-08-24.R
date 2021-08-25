library(tidyverse)
library(lubridate)
library(patchwork)
lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

# Plot 1
lemurs %>%
  mutate(birth_year = year(dob)) %>%
  group_by(birth_year, sex) %>%
  count(sort = TRUE) %>%
  filter(sex != "ND") %>%
  ggplot(aes(birth_year, n)) +
  geom_line() +
  geom_point()+
  theme_bw() +
  labs(x = "Birth Year", y = "Birth Count", title = "Yearly Birth And Death Counts") +
  facet_grid(sex~.,labeller = labeller(sex = c(F = "Female", M = "Male"))) +
lemurs %>%
  mutate(death_year = year(dod)) %>%
  group_by(death_year, sex) %>%
  count(sort = TRUE) %>%
  filter(sex != "ND") %>%
  filter(!is.na(death_year)) %>%
  ggplot(aes(death_year, n)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "Death Year", y = "Death Count")+
  facet_grid(sex~.,labeller = labeller(sex = c(F = "Female", M = "Male")))

#ggsave("Yearly Birth And Death Counts.png", width = 20, height = 10)

# Plot 2


bind_rows(
  lemurs %>%
    mutate(year = year(dob)) %>%
    group_by(year, sex) %>%
    count(sort = TRUE) %>%
    filter(sex != "ND") %>%
    mutate(status = "Birth"),
  lemurs %>%
    mutate(year = year(dod)) %>%
    group_by(year, sex) %>%
    count(sort = TRUE) %>%
    filter(sex != "ND") %>%
    mutate(status = "Death")
) %>%
  ungroup() %>%
  filter(!is.na(year)) %>%
  ggplot(aes(year, n, color = status)) +
  geom_line(size = 1.2) +
  geom_point(size = 1.5) +
  facet_grid(sex ~ .,labeller = labeller(sex = c(F = "Female", M = "Male"))) +
  labs(color = "", y = "Count", title = "Lemur Yearly Birth And Death Count") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 13, face = "bold")
  )

#ggsave("Lemur Yearly Birth And Death Count.png", width = 20, height = 10)

# Plot 3

lemurs %>%
  count(birth_institution, sex, sort = TRUE) %>%
  mutate(birth_institution = fct_lump(birth_institution, n = 8, w = n),
         birth_institution = fct_reorder(birth_institution, n)) %>%
  ggplot(aes(birth_institution, n, fill = sex)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_bw() +
  theme(
    legend.position = "bottom"
    
  ) +
  labs(x = "", y = "Count", title = "Male & Female Birth Counts From Top 8 Birth Instuitutions")

#ggsave("Male & Female Birth Counts From Top 8 Birth Instuitutions.png", width = 20, height = 10)

# Plot 4
lemurs %>%
  filter(sex != "ND") %>%
  count(name, sex, sort = TRUE) %>%
  mutate(name = fct_lump(name, n = 20, w = n),
         name = fct_reorder(name, n)) %>%
  ggplot(aes(name, n, fill = n)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 13, face = "bold"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13)
  ) +
  facet_wrap(~sex, scales = "free_y",labeller = labeller(sex = c(F = "Female", M = "Male"))) +
  labs(fill = "count", y = "count", title = "The Most Popular Male & Female Names")

#ggsave("The Most Popular Male & Female Names.png", width = 20, height = 10)


# lemurs %>%
#   filter(sex != "ND") %>%
#   group_by(birth_institution,sex) %>%
#   summarize(avg_age = mean(age_at_death_y, na.rm = TRUE)) %>%
#   ungroup() %>%
#   filter(!is.na(avg_age)) %>% 
#   #mutate(birth_institution = fct_reorder(birth_institution, avg_age)) %>%
#   arrange(desc(avg_age)) %>%
#   #top_n(10) %>%
#   ggplot(aes(birth_institution, avg_age, fill = sex)) +
#   geom_col(position = "dodge") +
#  # facet_wrap(~sex) +
#   coord_flip() +
#   theme_bw() +
#   theme(
#     legend.position = "bottom"
#   )








