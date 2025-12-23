# Associate (Patient, Time) pairs to corresponding index in the model

Associate (Patient, Time) pairs to corresponding index in the model

## Usage

``` r
get_index(train, test = NULL)

get_index2(t_max)
```

## Arguments

- train:

  Training dataframe

- test:

  Testing dataframe

- t_max:

  Vector indicating the length of each patient time-series

## Value

Dataframe with columns Patient, Time, Index

## Details

These functions are not designed for use with the Markov Chain model
(MC).

## Examples

``` r
library(dplyr)
id <- get_index2(t_max = rpois(10, 20))
df <- id %>% select(-Index) %>% slice_sample(prop = 0.9) %>% arrange(Patient, Time)
get_index(train = df)
#> # A tibble: 189 × 3
#>    Patient  Time Index
#>      <int> <int> <int>
#>  1       1     1     1
#>  2       1     2     2
#>  3       1     3     3
#>  4       1     4     4
#>  5       1     5     5
#>  6       1     6     6
#>  7       1     7     7
#>  8       1     8     8
#>  9       1     9     9
#> 10       1    10    10
#> # ℹ 179 more rows
```
