library(tidyverse)
theme_set(theme_light())

cran <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-19/cran_20230905.csv') |>
  janitor::clean_names()
package_authors <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-19/package_authors.csv')
cran_graph_nodes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-19/cran_graph_nodes.csv')
cran_graph_edges <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-19/cran_graph_edges.csv')

# Plot 1
cran |>
  count(license, sort = T, name = "count") |>
  mutate(license = fct_lump(license, n = 10, w = count)) |>
  group_by(license) |>
  summarize(count = sum(count), .groups = "drop") |>
  mutate(license = fct_reorder(license, count)) |>
  ggplot(aes(count, license, fill = license)) +
  geom_col(show.legend = F) +
  labs(y = "",
       title = "Top 10 Most Common CRAN Licenses")

#ggsave("plot1.png", width = 6)

# Plot 2
package_authors |>
  count(authorsR, sort = T) |>
  head(10) |>
  mutate(authorsR = fct_reorder(authorsR, n)) |>
  ggplot(aes(n, authorsR, color = authorsR)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_errorbarh(aes(xmin = 0,
                     xmax = n),
                 height = 0,
                 linewidth = 1) +
  theme(legend.position = "none") +
  labs(x = "package count",
       y = "",
       title = "Top 10 R Package Authors on CRAN")

#ggsave("plot2.png", width = 6)

# Plot 3
cran_graph_nodes |>
  ggplot(aes(x, y, color = dist2HW)) +
  geom_point(alpha = 0.5, size = 0.3) +
  scale_color_viridis_c() +
  theme_void() +
  labs(color = "distance to HW",
       title = "R Package Authors and Their Distance to Hadley Wickham") 

#ggsave("plot3.png", bg = "white", width = 6)

# Plot 4
cran |>
  mutate(date_publication = lubridate::ymd(str_remove(date_publication, "\\s.+$")),
         needs_compilation = paste("compilation:", needs_compilation)) |>
  ggplot(aes(date_publication)) +
  geom_histogram() +
  facet_wrap(~needs_compilation) +
  labs(x = "publication date",
       title = "R Package Publication and Compilation Status")

#ggsave("plot4.png")









  
