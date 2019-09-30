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
  read_excel("./data/HealthyHarborWaterWheel.xlsx", sheet=1)
```

    ## New names:
    ## * `` -> ...15

``` r
  skimr::skim(trash_wheel)
```

    ## Skim summary statistics
    ##  n obs: 336 
    ##  n variables: 15 
    ## 
    ## -- Variable type:character -------------------------------------------------------------------------------------
    ##  variable missing complete   n min max empty n_unique
    ##     ...15     318       18 336  35  35     0        1
    ##     Month       0      336 336   3  15     0       27
    ## 
    ## -- Variable type:numeric ---------------------------------------------------------------------------------------
    ##              variable missing complete   n     mean        sd      p0
    ##             Chip Bags       0      336 336  4887.13  30002.33  330   
    ##       Cigarette Butts       0      336 336 91871.43 569475.24 1000   
    ##              Dumpster      51      285 336   143        82.42    1   
    ##         Glass Bottles       0      336 336    71.21    438.08    2   
    ##          Grocery Bags       0      336 336  3610.51  22209.68   50   
    ##        Homes Powered*       0      336 336   106.7     659.77    0   
    ##       Plastic Bottles       0      336 336  5010.54  30737.97  210   
    ##           Polystyrene       0      336 336  5905.69  36221.48  320   
    ##          Sports Balls       0      336 336    32.22    198.92    0   
    ##  Volume (cubic yards)       0      336 336    39.63    243.53    7   
    ##         Weight (tons)       0      336 336     8.35     51.36    0.96
    ##                  Year      51      285 336  2016.08      1.4  2014   
    ##       p25      p50      p75      p100     hist
    ##   1168     2140     3140    547359    <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
    ##  14000    31500    65500     1e+07    <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
    ##     72      143      214       285    <U+2587><U+2587><U+2587><U+2587><U+2587><U+2587><U+2587><U+2587>
    ##     15.75    32       48      7975    <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
    ##    743     1530     2595     4e+05    <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
    ##     32.38    52.83    65     11950.67 <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
    ##   1205     2247     3220    561180    <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
    ##   1457.5   2575     3750    661437    <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
    ##      6       12       24      3608.6  <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
    ##     15       15       18      4439    <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
    ##      2.87     3.45     4.16    934.94 <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
    ##   2015     2016     2017      2018    <U+2585><U+2587><U+2581><U+2586><U+2581><U+2586><U+2581><U+2587>
    ## 
    ## -- Variable type:POSIXct ---------------------------------------------------------------------------------------
    ##  variable missing complete   n        min        max     median n_unique
    ##      Date      51      285 336 2014-05-16 2018-07-28 2016-07-13      186

``` r
#cleaning names
trash_wheel = 
  janitor::clean_names(trash_wheel)
  names(trash_wheel)
```

    ##  [1] "dumpster"           "month"              "year"              
    ##  [4] "date"               "weight_tons"        "volume_cubic_yards"
    ##  [7] "plastic_bottles"    "polystyrene"        "cigarette_butts"   
    ## [10] "glass_bottles"      "grocery_bags"       "chip_bags"         
    ## [13] "sports_balls"       "homes_powered"      "x15"

``` r
#removing 'total' rows, x15 column   
trash_wheel=
  tidyr::drop_na(trash_wheel, dumpster)

#removing x15 column, rounding sports balls
trash_wheel=
  select(trash_wheel,-x15)
  mutate(trash_wheel, sports_balls = as.integer(sports_balls))
```

    ## # A tibble: 285 x 14
    ##    dumpster month  year date                weight_tons volume_cubic_ya~
    ##       <dbl> <chr> <dbl> <dttm>                    <dbl>            <dbl>
    ##  1        1 May    2014 2014-05-16 00:00:00        4.31               18
    ##  2        2 May    2014 2014-05-16 00:00:00        2.74               13
    ##  3        3 May    2014 2014-05-16 00:00:00        3.45               15
    ##  4        4 May    2014 2014-05-17 00:00:00        3.1                15
    ##  5        5 May    2014 2014-05-17 00:00:00        4.06               18
    ##  6        6 May    2014 2014-05-20 00:00:00        2.71               13
    ##  7        7 May    2014 2014-05-21 00:00:00        1.91                8
    ##  8        8 May    2014 2014-05-28 00:00:00        3.7                16
    ##  9        9 June   2014 2014-06-05 00:00:00        2.52               14
    ## 10       10 June   2014 2014-06-11 00:00:00        3.76               18
    ## # ... with 275 more rows, and 8 more variables: plastic_bottles <dbl>,
    ## #   polystyrene <dbl>, cigarette_butts <dbl>, glass_bottles <dbl>,
    ## #   grocery_bags <dbl>, chip_bags <dbl>, sports_balls <int>,
    ## #   homes_powered <dbl>
