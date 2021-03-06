Tidytuesday
-----------

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.1.0     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
library(dplyr)
library(sf)
```

    ## Linking to GEOS 3.9.0, GDAL 3.2.1, PROJ 7.2.1

``` r
library(maps)
```

    ## 
    ## Attaching package: 'maps'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     map

``` r
library(tigris)
```

    ## To enable 
    ## caching of data, set `options(tigris_use_cache = TRUE)` in your R script or .Rprofile.

``` r
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

Data Exploriation
=================

``` r
bb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   ST = col_character(),
    ##   `COUNTY ID` = col_double(),
    ##   `COUNTY NAME` = col_character(),
    ##   `BROADBAND AVAILABILITY PER FCC` = col_character(),
    ##   `BROADBAND USAGE` = col_character()
    ## )

``` r
bb_ohio <- bb %>% filter(ST == "OH")
head(bb_ohio)
```

    ## # A tibble: 6 x 5
    ##   ST    `COUNTY ID` `COUNTY NAME`   `BROADBAND AVAILABILITY PE~ `BROADBAND USAG~
    ##   <chr>       <dbl> <chr>           <chr>                       <chr>           
    ## 1 OH          39001 Adams County    0.51                        0.15            
    ## 2 OH          39003 Allen County    0.96                        0.36            
    ## 3 OH          39005 Ashland County  0.81                        0.45            
    ## 4 OH          39007 Ashtabula Coun~ 0.92                        0.37            
    ## 5 OH          39009 Athens County   0.74                        0.18            
    ## 6 OH          39011 Auglaize County 0.94                        0.39

``` r
dim(bb_ohio)
```

    ## [1] 88  5

``` r
bb_zip <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband_zip.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   ST = col_character(),
    ##   `COUNTY NAME` = col_character(),
    ##   `COUNTY ID` = col_double(),
    ##   `POSTAL CODE` = col_double(),
    ##   `BROADBAND USAGE` = col_double(),
    ##   `ERROR RANGE (MAE)(+/-)` = col_double(),
    ##   `ERROR RANGE (95%)(+/-)` = col_double(),
    ##   MSD = col_double()
    ## )

``` r
bb_zip_ohio <- bb_zip %>% filter(ST == "OH") 
head(bb_zip_ohio)
```

    ## # A tibble: 6 x 8
    ##   ST    `COUNTY NAME` `COUNTY ID` `POSTAL CODE` `BROADBAND USAGE`
    ##   <chr> <chr>               <dbl>         <dbl>             <dbl>
    ## 1 OH    Adams               39001         45679             0.157
    ## 2 OH    Adams               39001         45697             0.161
    ## 3 OH    Adams               39001         45693             0.308
    ## 4 OH    Adams               39001         45618             1    
    ## 5 OH    Adams               39001         45650             0.121
    ## 6 OH    Adams               39001         45144             0.056
    ## # ... with 3 more variables: ERROR RANGE (MAE)(+/-) <dbl>,
    ## #   ERROR RANGE (95%)(+/-) <dbl>, MSD <dbl>

``` r
dim(bb_zip_ohio)
```

    ## [1] 1187    8

``` r
# code is referenced from https://github.com/andybridger/TidyTuesday/blob/main/2021w20/2021w20.R
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
OH_data <- filter(bb, ST == "OH")
OH_data$COUNTYFP <- substrRight(as.character(OH_data$`COUNTY ID`), 3)
tracts_max <- tracts(state = 'OH')
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |=======                                                               |  11%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |==============                                                        |  21%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |================                                                      |  24%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |=====================                                                 |  31%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |===================================                                   |  51%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |==========================================                            |  61%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |===================================================                   |  74%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  80%  |                                                                              |========================================================              |  81%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |===============================================================       |  91%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================|  99%  |                                                                              |======================================================================| 100%

``` r
ggtract_OH<-fortify(tracts_max, region = "GEOID") 
ggtract_OH<-left_join(ggtract_OH, OH_data, by=c("COUNTYFP")) 
```

``` r
ggplot() +
  geom_sf(data = ggtract_OH, aes(fill=as.numeric(`BROADBAND AVAILABILITY PER FCC`)), colour = "#061541", size =0.25) +
  coord_sf(datum = NA)+
  scale_fill_viridis_c(limits=c(0,1), 
                       breaks=c(0,1, 0.5), labels=c("0", "100%", "50%"),
                       guide = guide_colourbar(title.position = "top"))+
  theme_minimal(base_size=10)+
  theme(plot.subtitle = element_text(size=9),
        plot.title=element_text(size=14))+
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               reverse = FALSE,
                               barheight = unit(10,"lines"),
                               barwidth = unit(0.5, "lines")))+
  labs(fill="",
       title = "Ohio")
```

![](broadband_files/figure-markdown_github/unnamed-chunk-5-1.png)
