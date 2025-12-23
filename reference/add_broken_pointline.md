# Add broken pointline to ggplot

Add broken pointline to ggplot

## Usage

``` r
add_broken_pointline(df, aes_x = "x", aes_y = "y", size = 1, ...)
```

## Arguments

- df:

  Data with columns `aes_x` and `aes_y`.

- aes_x:

  Name of the `x` aesthetic.

- aes_y:

  Name of the `y` aesthetic.

- size:

  Size of the line and the points.

- ...:

  Other aesthetics to pass to
  [`ggplot2::aes_()`](https://ggplot2.tidyverse.org/reference/aes_.html).
  Valid aesthetics are the aesthetics for
  [`ggplot2::geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html)
  and
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
  (except `size` that is fixed). NB: if we want the colour to change
  with `Group` we would need to add `colour = as.name(Group)`.

## Value

List to add to ggplot.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)

df1 <- tibble(x = 1:100, y = cumsum(rnorm(100))) %>%
  slice_sample(prop = .8) %>%
  arrange(x)

ggplot() +
  add_broken_pointline(df1) +
  theme_bw(base_size = 15)
#> Warning: `aes_()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`
#> ℹ The deprecated feature was likely used in the EczemaPred package.
#>   Please report the issue at <https://github.com/ghurault/EczemaPred/issues>.
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the EczemaPred package.
#>   Please report the issue at <https://github.com/ghurault/EczemaPred/issues>.
#> Warning: Removed 20 rows containing missing values or values outside the scale range
#> (`geom_point()`).


df2 <- mutate(df1, Group = case_when(x < 60 ~ "A", TRUE ~ "B"))

ggplot() +
  add_broken_pointline(df2, colour = as.name("Group")) +
  scale_colour_discrete(na.translate = FALSE) +
  theme_bw(base_size = 15)
#> Warning: Removed 20 rows containing missing values or values outside the scale range
#> (`geom_path()`).
#> Warning: Removed 20 rows containing missing values or values outside the scale range
#> (`geom_point()`).

```
