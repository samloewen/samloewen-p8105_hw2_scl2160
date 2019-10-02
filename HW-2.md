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
  read_excel("./data/Trash-Wheel-Collection-Totals-8-6-19.xlsx", sheet = 1, skip = 1) %>% 
  janitor::clean_names() %>% 
  drop_na(dumpster) %>% 
  select(-x15, -x16, -x17) %>% 
  mutate(sports_balls = as.integer(sports_balls))
```

    ## New names:
    ## * `` -> ...15
    ## * `` -> ...16
    ## * `` -> ...17

``` r
trash_wheel
```

    ## # A tibble: 344 x 14
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
    ## # ... with 334 more rows, and 8 more variables: plastic_bottles <dbl>,
    ## #   polystyrene <dbl>, cigarette_butts <dbl>, glass_bottles <dbl>,
    ## #   grocery_bags <dbl>, chip_bags <dbl>, sports_balls <int>,
    ## #   homes_powered <dbl>

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
  full_join(precip_17, precip_18) %>% 
  mutate(month = month.name[month])
```

    ## Joining, by = c("year", "month", "total")

``` r
precip_all
```

    ## # A tibble: 24 x 3
    ##     year month     total
    ##    <dbl> <chr>     <dbl>
    ##  1  2017 January    2.34
    ##  2  2017 February   1.46
    ##  3  2017 March      3.57
    ##  4  2017 April      3.99
    ##  5  2017 May        5.64
    ##  6  2017 June       1.4 
    ##  7  2017 July       7.09
    ##  8  2017 August     4.44
    ##  9  2017 September  1.95
    ## 10  2017 October    0   
    ## # ... with 14 more rows

Write a paragraph about these data; you are encouraged to use inline R.
Be sure to note the number of observations in both resulting datasets,
and give examples of key variables. For available data, what was the
total precipitation in 2018? What was the median number of sports balls
in a dumpster in 2017?

There are x observations in the trash\_wheel dataset and x in the
precip\_all dataset.

My trash\_wheel data set has 344 observations, and my precip\_all data
set has 24 observations. Trash\_wheel includes variables to show the
number of items removed from the harbor, such as `glass_bottles` and
`grocery_bags`. We can also see that the total number of sports ball in
a dumpster in 2017 was r sum(pull(trash\_wheel, dumpster)).

The precip\_all dataframe can show us that the total precipitation in
2018 was r sum(pull(precip\_all, dumpster)).

## Problem 2

First, clean the data in pols-month.csv. Use separate() to break up the
variable mon into integer variables year, month, and day; replace month
number with month name; create a president variable taking values gop
and dem, and remove prez\_dem and prez\_gop; and remove the day
variable.

``` r
pols =
  read_csv("./data/pols-month.csv") %>%
  janitor::clean_names() %>% 
  separate(mon, c("year", "month", "day")) %>%
  mutate(prez = ifelse(prez_gop == 1, "gop", "dem")) %>% 
  mutate(year = as.numeric(year)) %>% 
  select (-prez_gop, -prez_dem, -day)
```

    ## Parsed with column specification:
    ## cols(
    ##   mon = col_date(format = ""),
    ##   prez_gop = col_double(),
    ##   gov_gop = col_double(),
    ##   sen_gop = col_double(),
    ##   rep_gop = col_double(),
    ##   prez_dem = col_double(),
    ##   gov_dem = col_double(),
    ##   sen_dem = col_double(),
    ##   rep_dem = col_double()
    ## )

``` r
pols
```

    ## # A tibble: 822 x 9
    ##     year month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem prez 
    ##    <dbl> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>
    ##  1  1947 01         23      51     253      23      45     198 dem  
    ##  2  1947 02         23      51     253      23      45     198 dem  
    ##  3  1947 03         23      51     253      23      45     198 dem  
    ##  4  1947 04         23      51     253      23      45     198 dem  
    ##  5  1947 05         23      51     253      23      45     198 dem  
    ##  6  1947 06         23      51     253      23      45     198 dem  
    ##  7  1947 07         23      51     253      23      45     198 dem  
    ##  8  1947 08         23      51     253      23      45     198 dem  
    ##  9  1947 09         23      51     253      23      45     198 dem  
    ## 10  1947 10         23      51     253      23      45     198 dem  
    ## # ... with 812 more rows

Second, clean the data in snp.csv using a similar process to the above.
For consistency across datasets, arrange according to year and month,
and organize so that year and month are the leading columns.

``` r
snp =
  read_csv("./data/snp.csv") %>%
  janitor::clean_names() %>% 
  separate(date, c("month", "day", "year")) %>% 
  mutate(year = as.numeric(year)) %>% 
  select (year, month, -day, close) %>% 
  arrange (year, month)
```

    ## Parsed with column specification:
    ## cols(
    ##   date = col_character(),
    ##   close = col_double()
    ## )

``` r
#fix the ascending order#
snp
```

    ## # A tibble: 787 x 3
    ##     year month close
    ##    <dbl> <chr> <dbl>
    ##  1  1950 1      17.0
    ##  2  1950 10     19.5
    ##  3  1950 11     19.5
    ##  4  1950 12     20.4
    ##  5  1950 2      17.2
    ##  6  1950 3      17.3
    ##  7  1950 4      18.0
    ##  8  1950 5      18.8
    ##  9  1950 6      17.7
    ## 10  1950 7      17.8
    ## # ... with 777 more rows

Third, tidy the unemployment data so that it can be merged with the
previous datasets. This process will involve switching from “wide” to
“long” format; ensuring that key variables have the same name; and
ensuring that key variables take the same values.

``` r
unemp=
  read_csv("./data/unemployment.csv") %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    jan:dec, 
    names_to = "month", 
    values_to = "snp")
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_double(),
    ##   Jan = col_double(),
    ##   Feb = col_double(),
    ##   Mar = col_double(),
    ##   Apr = col_double(),
    ##   May = col_double(),
    ##   Jun = col_double(),
    ##   Jul = col_double(),
    ##   Aug = col_double(),
    ##   Sep = col_double(),
    ##   Oct = col_double(),
    ##   Nov = col_double(),
    ##   Dec = col_double()
    ## )

``` r
unemp
```

    ## # A tibble: 816 x 3
    ##     year month   snp
    ##    <dbl> <chr> <dbl>
    ##  1  1948 jan     3.4
    ##  2  1948 feb     3.8
    ##  3  1948 mar     4  
    ##  4  1948 apr     3.9
    ##  5  1948 may     3.5
    ##  6  1948 jun     3.6
    ##  7  1948 jul     3.6
    ##  8  1948 aug     3.9
    ##  9  1948 sep     3.8
    ## 10  1948 oct     3.7
    ## # ... with 806 more rows

Join the datasets by merging snp into pols, and merging unemployment
into the result.

``` r
q2_merge =
  left_join(pols, snp, by = c("year","month"))
q2_merge
```

    ## # A tibble: 822 x 10
    ##     year month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem prez  close
    ##    <dbl> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr> <dbl>
    ##  1  1947 01         23      51     253      23      45     198 dem      NA
    ##  2  1947 02         23      51     253      23      45     198 dem      NA
    ##  3  1947 03         23      51     253      23      45     198 dem      NA
    ##  4  1947 04         23      51     253      23      45     198 dem      NA
    ##  5  1947 05         23      51     253      23      45     198 dem      NA
    ##  6  1947 06         23      51     253      23      45     198 dem      NA
    ##  7  1947 07         23      51     253      23      45     198 dem      NA
    ##  8  1947 08         23      51     253      23      45     198 dem      NA
    ##  9  1947 09         23      51     253      23      45     198 dem      NA
    ## 10  1947 10         23      51     253      23      45     198 dem      NA
    ## # ... with 812 more rows

``` r
final_db = 
  left_join(q2_merge,unemp, by =c("year","month"))
final_db
```

    ## # A tibble: 822 x 11
    ##     year month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem prez  close
    ##    <dbl> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr> <dbl>
    ##  1  1947 01         23      51     253      23      45     198 dem      NA
    ##  2  1947 02         23      51     253      23      45     198 dem      NA
    ##  3  1947 03         23      51     253      23      45     198 dem      NA
    ##  4  1947 04         23      51     253      23      45     198 dem      NA
    ##  5  1947 05         23      51     253      23      45     198 dem      NA
    ##  6  1947 06         23      51     253      23      45     198 dem      NA
    ##  7  1947 07         23      51     253      23      45     198 dem      NA
    ##  8  1947 08         23      51     253      23      45     198 dem      NA
    ##  9  1947 09         23      51     253      23      45     198 dem      NA
    ## 10  1947 10         23      51     253      23      45     198 dem      NA
    ## # ... with 812 more rows, and 1 more variable: snp <dbl>

Write a short paragraph about these datasets. Explain briefly what each
dataset contained, and describe the resulting dataset (e.g. give the
dimension, range of years, and names of key variables).

## Problem 3

Load and tidy the data. Note that, although these data may seem fairly
well formatted initially, the names of a categorical predictor and the
case structure of string variables changed over time; you’ll need to
address this in your data cleaning. Also, some rows seem duplicated, and
these will need to be removed (hint: google something like “dplyr remove
duplicate rows” to get started).

``` r
baby_names = 
  read_csv("./data/Popular_Baby_Names.csv") %>% 
  janitor::clean_names() %>% 
  mutate(ethnicity = replace(ethnicity, ethnicity == "ASIAN AND PACI", "ASIAN AND PACIFIC ISLANDER")) %>% 
  mutate(ethnicity = replace(ethnicity, ethnicity == "BLACK NON HISP", "BLACK NON HISPANIC")) %>% 
  mutate(ethnicity = replace(ethnicity, ethnicity == "WHITE NON HISP", "WHITE NON HISPANIC")) %>% 
  mutate(childs_first_name = toupper(childs_first_name)) %>% 
  distinct()
```

    ## Parsed with column specification:
    ## cols(
    ##   `Year of Birth` = col_double(),
    ##   Gender = col_character(),
    ##   Ethnicity = col_character(),
    ##   `Child's First Name` = col_character(),
    ##   Count = col_double(),
    ##   Rank = col_double()
    ## )

``` r
unique(baby_names$ethnicity)
```

    ## [1] "ASIAN AND PACIFIC ISLANDER" "BLACK NON HISPANIC"        
    ## [3] "HISPANIC"                   "WHITE NON HISPANIC"

Produce a well-structured, reader-friendly table showing the rank in
popularity of the name “Olivia” as a female baby name over time; this
should have rows for ethnicities and columns for year. Produce a similar
table showing the most popular name among male children over time.

Finally, for male, white non-hispanic children born in 2016, produce a
scatter plot showing the number of children with a name (y axis) against
the rank in popularity of that name (x axis).
