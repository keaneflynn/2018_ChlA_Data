SRF-2018-Code
================
Keane Flynn & Weston Slaughter
12/30/2018

Load Libraries
--------------

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(readr)
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ stringr 1.3.1
    ## ✔ tidyr   0.8.1     ✔ forcats 0.3.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
```

ChlA Data
=========

``` r
ChlA_Data_Porter <- 
  readr::read_csv(file = "ChlA-Data/ChlA-Data.csv",
                  col_types = "ccccdcddddddddc") %>%
  filter(Site == "Porter_Above") %>%
  group_by(Date_collected, Site_1)
```

    ## Warning: Duplicated column names deduplicated: 'Site' => 'Site_1' [4]

``` r
ChlA_Data_Porter
```

    ## # A tibble: 72 x 15
    ## # Groups:   Date_collected, Site_1 [20]
    ##    Site  Date_collected Date_processed Site_1 `Cobble#_SID` `Pool/Riffle`
    ##    <chr> <chr>          <chr>          <chr>          <dbl> <chr>        
    ##  1 Port… 5/1/18         5/2/18         Frog_…             1 riffle       
    ##  2 Port… 5/1/18         5/2/18         Frog_…             5 riffle       
    ##  3 Port… 5/1/18         5/2/18         Frog_…             7 pool         
    ##  4 Port… 5/1/18         5/2/18         Frog_…            11 pool         
    ##  5 Port… 5/1/18         5/2/18         Jesus…             8 riffle       
    ##  6 Port… 5/1/18         5/2/18         Jesus…             9 riffle       
    ##  7 Port… 5/1/18         5/2/18         Jesus…            15 pool         
    ##  8 Port… 5/1/18         5/2/18         Jesus…            16 pool         
    ##  9 Port… 5/1/18         5/2/18         Golf_…             4 riffle       
    ## 10 Port… 5/1/18         5/2/18         Golf_…             6 riffle       
    ## # ... with 62 more rows, and 9 more variables: `Volume (mL)` <dbl>,
    ## #   `Volume Added` <dbl>, `Canister_#` <dbl>, Chla_reading <dbl>,
    ## #   Total_Chla_extracted <dbl>, Total_Chla_in_Sample <dbl>,
    ## #   Chla_area <dbl>, `Cobble Area` <dbl>, Notes <chr>

Summarized ChlA Data
--------------------

``` r
Summarized_ChlA_Data <- ChlA_Data_Porter %>%
  group_by(Date_collected, Site_1) %>%
  summarise(mean_ChlA_area = mean(Chla_area))
Summarized_ChlA_Data
```

    ## # A tibble: 20 x 3
    ## # Groups:   Date_collected [?]
    ##    Date_collected Site_1           mean_ChlA_area
    ##    <chr>          <chr>                     <dbl>
    ##  1 5/1/18         Frog_Legs_18.1           0.389 
    ##  2 5/1/18         Golf_Ball_18.3           0.230 
    ##  3 5/1/18         Jesus_Toast_18.2         0.208 
    ##  4 5/1/18         Waterfall_18.4           0.205 
    ##  5 5/18/18        Frog_Legs_18.1           0.244 
    ##  6 5/18/18        Golf_Ball_18.3           0.116 
    ##  7 5/18/18        Jesus_Toast_18.2         0.157 
    ##  8 5/18/18        Waterfall_18.4           0.0860
    ##  9 6/13/18        Frog_Legs_18.1           0.236 
    ## 10 6/13/18        Golf_Ball_18.3           0.263 
    ## 11 6/13/18        Jesus_Toast_18.2         0.300 
    ## 12 6/13/18        Waterfall_18.4           0.249 
    ## 13 7/13/18        Frog_Legs_18.1           0.312 
    ## 14 7/13/18        Golf_Ball_18.3           0.384 
    ## 15 7/13/18        Jesus_Toast_18.2         0.546 
    ## 16 7/13/18        Waterfall_18.4           0.289 
    ## 17 8/8/18         Frog_Legs_18.1           0.379 
    ## 18 8/8/18         Golf_Ball_18.3           0.535 
    ## 19 8/8/18         Jesus_Toast_18.2         0.545 
    ## 20 8/8/18         Waterfall_18.4           0.609

Plotting ChlA vs Time
---------------------

``` r
ChlA_Graph <- Summarized_ChlA_Data %>%
  ggplot(aes(x = Date_collected, y = mean_ChlA_area)) +
  geom_line(aes(group = Site_1, color = Site_1)) +
  geom_point(aes(shape = Site_1)) +
  labs(x = "Date", y = "Average Chlorophyll-A Reading per Unit Area of Cobble") +
  ggtitle("Chlorophyll-A Measurements vs Time")
ChlA_Graph
```

![](SRF-2018-Code_files/figure-markdown_github/unnamed-chunk-4-1.png)

AFD Data
========

``` r
AFD_Data_Porter <-
  readr::read_csv(file = "ChlA-Data/AFD-Data.csv",
                  col_types = "ccccdcddddddddc") %>%
  filter(Site == "Porter_Above") %>%
  group_by(Date_collected, Site_1) %>%
  select(Date_collected, Site_1, Cobble_SID, Pool_or_Riffle, Dry_Weight_g, AFD_Weight_g, Difference)
```

    ## Warning: Missing column names filled in: 'X14' [14], 'X15' [15]

    ## Warning: Duplicated column names deduplicated: 'Site' => 'Site_1' [4]

    ## Warning in rbind(names(probs), probs_f): number of columns of result is not
    ## a multiple of vector length (arg 1)

    ## Warning: 19 parsing failures.
    ## row # A tibble: 5 x 5 col     row col   expected actual                            file              expected   <int> <chr> <chr>    <chr>                             <chr>             actual 1    30 Notes a double Odd donut shape in filter         'ChlA-Data/AFD-D… file 2    33 Notes a double very small amount of algae scrap… 'ChlA-Data/AFD-D… row 3    48 Notes a double in oven at 5:10 PM                'ChlA-Data/AFD-D… col 4    64 Notes a double in oven at 5:45 PM                'ChlA-Data/AFD-D… expected 5    69 Notes a double MELTED IN OVEN REDO               'ChlA-Data/AFD-D…
    ## ... ................. ... .......................................................................... ........ .......................................................................... ...... .......................................................................... .... .......................................................................... ... .......................................................................... ... .......................................................................... ........ ..........................................................................
    ## See problems(...) for more details.

``` r
AFD_Data_Porter
```

    ## # A tibble: 72 x 7
    ## # Groups:   Date_collected, Site_1 [21]
    ##    Date_collected Site_1 Cobble_SID Pool_or_Riffle Dry_Weight_g
    ##    <chr>          <chr>       <dbl> <chr>                 <dbl>
    ##  1 5/1/18         Frog_…          1 riffle                 1.14
    ##  2 5/1/18         Frog_…          5 riffle                 1.18
    ##  3 5/1/18         Frog_…          7 pool                   1.16
    ##  4 5/1/18         Frog_…         11 pool                   1.15
    ##  5 5/1/18         Jesus…          8 riffle                 1.14
    ##  6 5/1/18         Jesus…          9 riffle                 1.14
    ##  7 5/1/18         Jesus…         15 pool                   1.15
    ##  8 5/1/18         Jesus…         16 pool                   1.15
    ##  9 5/1/18         Golf_…          4 riffle                 1.15
    ## 10 5/1/18         Golf_…          6 riffle                 1.14
    ## # ... with 62 more rows, and 2 more variables: AFD_Weight_g <dbl>,
    ## #   Difference <dbl>

Summarizing AFD Data
--------------------

``` r
Summarized_AFD_Data <- AFD_Data_Porter %>%
  filter(!AFD_Weight_g == "NA") %>%
  group_by(Date_collected, Site_1) %>%
  summarise(mean_diff = mean(Difference))
Summarized_AFD_Data
```

    ## # A tibble: 21 x 3
    ## # Groups:   Date_collected [?]
    ##    Date_collected Site_1           mean_diff
    ##    <chr>          <chr>                <dbl>
    ##  1 5/1/18         Frog_Legs_18.1     0.004  
    ##  2 5/1/18         Golf_Ball_18.3     0.00222
    ##  3 5/1/18         Jesus_Toast_18.2   0.00228
    ##  4 5/1/18         Waterfall_18.4     0.00225
    ##  5 5/18/18        Frog_Legs_18.1     0.00288
    ##  6 5/18/18        Golf_Ball_18.3     0.0019 
    ##  7 5/18/18        Jesus_Toast_18.2   0.00282
    ##  8 5/18/18        Waterfall_18.4     0.00238
    ##  9 6/13/18        Frog_Legs_18.1     0.0018 
    ## 10 6/13/18        Golf_Ball_18.3     0.00267
    ## # ... with 11 more rows

Plotting AFD vs Time
--------------------

``` r
AFD_Graph <- Summarized_AFD_Data %>%
  ggplot(aes(x = Date_collected, y = mean_diff)) +
  geom_line(aes(group = Site_1, color = Site_1)) +
  geom_point(aes(shape = Site_1)) +
  labs(x = "Date", y = "Average Chlorophyll Mass Collected per Cobble") +
  ggtitle("Ash Free Dry Mass Measurements vs Time")
AFD_Graph
```

![](SRF-2018-Code_files/figure-markdown_github/unnamed-chunk-7-1.png)

Dissolved Oxygen Data
=====================
