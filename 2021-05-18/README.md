Data Wrangling and Visualization on Manager Survey
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

Since we are in the U.S., it would be interesting to see the average
`annual_salary` in each state. As we can see from the following code,
the dataset on the U.S. states is super messy and so many “states” do
not make any sense.

``` r
unique(survey_us$state)
```

    ##  [1] "Massachusetts"                                                        
    ##  [2] "Arizona"                                                              
    ##  [3] "Florida"                                                              
    ##  [4] NA                                                                     
    ##  [5] "Michigan"                                                             
    ##  [6] "Minnesota"                                                            
    ##  [7] "Ohio"                                                                 
    ##  [8] "Texas"                                                                
    ##  [9] "District of Columbia"                                                 
    ## [10] "New York"                                                             
    ## [11] "Pennsylvania"                                                         
    ## [12] "California"                                                           
    ## [13] "Illinois"                                                             
    ## [14] "South Carolina"                                                       
    ## [15] "Georgia"                                                              
    ## [16] "Oregon"                                                               
    ## [17] "Indiana"                                                              
    ## [18] "Tennessee"                                                            
    ## [19] "North Carolina"                                                       
    ## [20] "Missouri"                                                             
    ## [21] "New Jersey"                                                           
    ## [22] "Virginia"                                                             
    ## [23] "Oklahoma"                                                             
    ## [24] "Wisconsin"                                                            
    ## [25] "Nebraska"                                                             
    ## [26] "Washington"                                                           
    ## [27] "Connecticut"                                                          
    ## [28] "Maryland"                                                             
    ## [29] "Colorado"                                                             
    ## [30] "Montana"                                                              
    ## [31] "Iowa"                                                                 
    ## [32] "New Hampshire"                                                        
    ## [33] "North Dakota"                                                         
    ## [34] "Kansas"                                                               
    ## [35] "Arkansas"                                                             
    ## [36] "Idaho"                                                                
    ## [37] "Nevada"                                                               
    ## [38] "Delaware"                                                             
    ## [39] "Vermont"                                                              
    ## [40] "Kentucky"                                                             
    ## [41] "New Mexico"                                                           
    ## [42] "Hawaii"                                                               
    ## [43] "Rhode Island"                                                         
    ## [44] "Utah"                                                                 
    ## [45] "Maine"                                                                
    ## [46] "Louisiana"                                                            
    ## [47] "Mississippi"                                                          
    ## [48] "Alabama"                                                              
    ## [49] "District of Columbia, Virginia"                                       
    ## [50] "West Virginia"                                                        
    ## [51] "Georgia, New York"                                                    
    ## [52] "California, Oregon"                                                   
    ## [53] "Kentucky, Ohio"                                                       
    ## [54] "North Carolina, Utah"                                                 
    ## [55] "Alaska"                                                               
    ## [56] "California, Pennsylvania"                                             
    ## [57] "Georgia, Washington"                                                  
    ## [58] "Massachusetts, Rhode Island"                                          
    ## [59] "South Dakota"                                                         
    ## [60] "Wyoming"                                                              
    ## [61] "District of Columbia, Maryland"                                       
    ## [62] "Massachusetts, Pennsylvania"                                          
    ## [63] "New Jersey, Virginia"                                                 
    ## [64] "Massachusetts, Vermont"                                               
    ## [65] "New Jersey, Pennsylvania"                                             
    ## [66] "Iowa, Utah, Vermont"                                                  
    ## [67] "Texas, Virginia"                                                      
    ## [68] "Utah, Vermont"                                                        
    ## [69] "California, Texas"                                                    
    ## [70] "Indiana, Ohio"                                                        
    ## [71] "Ohio, Washington"                                                     
    ## [72] "Kansas, Missouri"                                                     
    ## [73] "Colorado, Illinois"                                                   
    ## [74] "California, New Jersey"                                               
    ## [75] "Louisiana, Washington"                                                
    ## [76] "District of Columbia, Washington"                                     
    ## [77] "New York, Oregon, Vermont"                                            
    ## [78] "Iowa, Nebraska"                                                       
    ## [79] "California, District of Columbia, Illinois, Iowa, Maryland, Minnesota"

``` r
survey_us %>% filter(str_detect(state, ", ", negate = TRUE)) %>% 
  group_by(state) %>%
  summarize(mean_salary = mean(annual_salary)) %>%
  ggplot(aes(state, mean_salary, fill = state)) +
  geom_col(width = 0.5) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 7)) +
  xlab("") +
  ylab("Average Salary") +
  coord_flip()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Surprisingly, Alabama has the hightest average salary.

``` r
survey_us$highest_level_of_education_completed <- factor(survey_us$highest_level_of_education_completed, levels = c(
"High School", "Some college", "College degree", "Master's degree", "PhD", "Professional degree (MD, JD, etc.)", "NA" 
))
survey_us %>% group_by(highest_level_of_education_completed, gender) %>%
  summarize(mean_salary = mean(annual_salary)) %>%
  ggplot(aes(highest_level_of_education_completed, mean_salary, fill = highest_level_of_education_completed)) +
  geom_col(width = 0.5) +
  facet_wrap(~gender) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(size = 8),
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 8)) +
  scale_x_discrete(labels = c("HS", "SC", "Col", "Mas", "PhD", "Pro", "NA")) + 
  xlab("") +
  ylab("Average Salary") +
  ggtitle("Gender-Wise Average Salary in the U.S.")
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
survey_us %>% group_by(highest_level_of_education_completed, gender) %>%
  summarize(annual_salary) %>% filter(annual_salary <= 300000) %>%
  ggplot(aes(highest_level_of_education_completed, annual_salary, fill = highest_level_of_education_completed)) +
  geom_boxplot() +
  facet_wrap(~gender) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(size = 7),
        legend.title = element_blank(),
        axis.ticks = element_blank()) +
  xlab("") +
  ylab("Average Salary")  +
  scale_x_discrete(labels = c("HS", "SC", "Col", "Mas", "PhD", "Pro", "NA")) 
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
