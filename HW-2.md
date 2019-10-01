HW 2
================
Sam Loewen
9/27/2019

## Problem 1

First I will read and clean the Mr. Trash Wheel sheet:

  - specify the sheet in the Excel file and to omit non-data entries
    (rows with notes / figures; columns containing notes) using
    arguments in read\_excel
  - use reasonable variable names
  - omit rows that do not include dumpster-specific data
  - round the number of sports balls to the nearest integer and converts
    the result to an integer variable (using as.integer)

<!-- end list -->

``` r
trash_wheel = 
  read_excel("./data/Trash-Wheel-Collection-Totals-8-6-19.xlsx", sheet = 1) %>% 
  janitor::clean_names() %>% 
  drop_na(dumpster) %>% 
  select(-x15) %>% 
  mutate(sports_balls = as.integer(sports_balls))
```

    ## New names:
    ## * `` -> ...15
    ## * `` -> ...16
    ## * `` -> ...17

Next I will read and clean precipitation data for 2017 and 2018. For
each omit rows without precipitation data and add a variable year. Next,
combine precipitation datasets and

convert month to a character variable (the variable month.name is built
into R and should be useful).

``` r
precip_17 = 
  read_excel("./data/Trash-Wheel-Collection-Totals-8-6-19.xlsx", sheet = 6, skip = 1) %>% 
  janitor::clean_names() %>% 
  drop_na() %>% 
  mutate(year = 2017) %>% 
  select(year, month, total)
precip_17
```

    ## # A tibble: 12 x 3
    ##     year month total
    ##    <dbl> <dbl> <dbl>
    ##  1  2017     1  2.34
    ##  2  2017     2  1.46
    ##  3  2017     3  3.57
    ##  4  2017     4  3.99
    ##  5  2017     5  5.64
    ##  6  2017     6  1.4 
    ##  7  2017     7  7.09
    ##  8  2017     8  4.44
    ##  9  2017     9  1.95
    ## 10  2017    10  0   
    ## 11  2017    11  0.11
    ## 12  2017    12  0.94

``` r
precip_18 = 
  read_excel("./data/Trash-Wheel-Collection-Totals-8-6-19.xlsx", sheet = 5, skip = 1) %>% 
  janitor::clean_names() %>% 
  drop_na() %>% 
  mutate(year = 2018) %>% 
  select(year, month, total)
precip_18
```

    ## # A tibble: 12 x 3
    ##     year month total
    ##    <dbl> <dbl> <dbl>
    ##  1  2018     1  0.94
    ##  2  2018     2  4.8 
    ##  3  2018     3  2.69
    ##  4  2018     4  4.69
    ##  5  2018     5  9.27
    ##  6  2018     6  4.77
    ##  7  2018     7 10.2 
    ##  8  2018     8  6.45
    ##  9  2018     9 10.5 
    ## 10  2018    10  2.12
    ## 11  2018    11  7.82
    ## 12  2018    12  6.11

``` r
precip_all = 
  full_join(precip_17, precip_18)
```

    ## Joining, by = c("year", "month", "total")

``` r
precip_all
```

    ## # A tibble: 24 x 3
    ##     year month total
    ##    <dbl> <dbl> <dbl>
    ##  1  2017     1  2.34
    ##  2  2017     2  1.46
    ##  3  2017     3  3.57
    ##  4  2017     4  3.99
    ##  5  2017     5  5.64
    ##  6  2017     6  1.4 
    ##  7  2017     7  7.09
    ##  8  2017     8  4.44
    ##  9  2017     9  1.95
    ## 10  2017    10  0   
    ## # ... with 14 more rows

Write a paragraph about these data; you are encouraged to use inline R.
Be sure to note the number of observations in both resulting datasets,
and give examples of key variables. For available data, what was the
total precipitation in 2018? What was the median number of sports balls
in a dumpster in 2017?

## Problem 2
