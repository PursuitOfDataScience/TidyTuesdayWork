Manager Survey
================
Y. Yu

``` r
library(tidyverse)
library(ggplot2)
library(dplyr)
library(knitr)
```

# Data Exploration

``` r
survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')
head(survey)
```

    ## # A tibble: 6 x 18
    ##   timestamp  how_old_are_you industry  job_title  additional_cont~ annual_salary
    ##   <chr>      <chr>           <chr>     <chr>      <chr>                    <dbl>
    ## 1 4/27/2021~ 25-34           Educatio~ Research ~ NA                       55000
    ## 2 4/27/2021~ 25-34           Computin~ Change & ~ NA                       54600
    ## 3 4/27/2021~ 25-34           Accounti~ Marketing~ NA                       34000
    ## 4 4/27/2021~ 25-34           Nonprofi~ Program M~ NA                       62000
    ## 5 4/27/2021~ 25-34           Accounti~ Accountin~ NA                       60000
    ## 6 4/27/2021~ 25-34           Educatio~ Scholarly~ NA                       62000
    ## # ... with 12 more variables: other_monetary_comp <chr>, currency <chr>,
    ## #   currency_other <chr>, additional_context_on_income <chr>, country <chr>,
    ## #   state <chr>, city <chr>, overall_years_of_professional_experience <chr>,
    ## #   years_of_experience_in_field <chr>,
    ## #   highest_level_of_education_completed <chr>, gender <chr>, race <chr>

``` r
dim(survey)
```

    ## [1] 26232    18

Let’s focus on the U.S. only for the moment.

``` r
survey_us <- survey %>%
  filter(country == "United States")
```

``` r
missing_values <- data.frame(col = names(survey_us),missing_value=colSums(is.na(survey_us)))
rownames(missing_values) <- 1:nrow(missing_values)
ggplot(missing_values, aes(col, missing_value, color = col, fill = col)) +
  geom_col() +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Column-wise Missing Value in the U.S.") +
  xlab("") + ylab("missing value count") + 
  coord_flip() 
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
