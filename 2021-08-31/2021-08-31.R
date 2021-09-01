library(tidyverse)
bird_baths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv')

skimr::skim(bird_baths)

# bird_baths %>%
#   count(survey_year, bioregions, sort = TRUE)
# 
# bird_baths %>%
#   count(bird_type, sort = TRUE)
# 
# bird_baths %>%
#   group_by(survey_year, bird_type) %>%
#   summarize(sum = sum(bird_count)) %>%
#   arrange(desc(sum))
# 
# bird_baths %>%
#   count(urban_rural, sort = TRUE)
# 
# bird_baths %>%
#   group_by(bioregions,bird_type) %>%
#   summarize(sum = sum(bird_count)) %>%
#   arrange(desc(sum)) 

# plot 1

bird_baths %>%
  group_by(survey_year, urban_rural, bioregions, bird_type) %>%
  summarize(total_count = sum(bird_count)) %>%
  ungroup() %>%
  mutate(bird_type = fct_lump(bird_type, n = 10, w = total_count)) %>%
  #arrange(desc(total_count))
  ggplot(aes(urban_rural, bird_type, size = total_count, color = total_count)) +
  geom_point() +
  facet_grid(survey_year ~ bioregions) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 7.5, face = "bold"), 
    axis.title = element_blank(),
    axis.text = element_text(face = "bold")) +
  labs(color = "total # of birds", size = "total # of birds", 
       title = "Top 10 Bird Type Counts between Rural & Urban Faceted by Region and Survey Year",
       subtitle = "NAs Included"
       ) +
  scale_size_continuous(breaks = seq(10, 300, by = 40)) +
  scale_color_continuous(breaks = seq(10, 300, by = 40))

#ggsave("NAs Included.png", width = 20, height = 10)
  

# plot 2 dropping na
bird_baths %>%
  drop_na() %>%
  group_by(survey_year, urban_rural, bioregions, bird_type) %>%
  summarize(total_count = sum(bird_count)) %>%
  ungroup() %>%
  mutate(bird_type = fct_lump(bird_type, n = 10, w = total_count)) %>%
  #arrange(desc(total_count))
  ggplot(aes(urban_rural, bird_type, size = total_count, color = total_count)) +
  geom_point() +
  facet_grid(survey_year ~ bioregions) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"), 
    axis.title = element_blank(),
    axis.text = element_text(face = "bold")) +
  labs(color = "total # of birds", size = "total # of birds", 
       title = "Top 10 Bird Type Counts between Rural & Urban Faceted by Region and Survey Year",
       subtitle = "NAs Excluded"
  ) +
  scale_size_continuous(breaks = seq(10, 110, by = 10)) +
  scale_color_continuous(breaks = seq(10, 110, by = 10))

#ggsave("NAs Excluded.png", width = 20, height = 10)  
  
  
  
  
  
  
  
  
  
  
