# Add fanchart to ggplot

The fanchart is obtained by overlaying
[`ggplot2::geom_ribbon()`](https://ggplot2.tidyverse.org/reference/geom_ribbon.html)
of different widths (corresponding to different levels). This function
is not a geom.

## Usage

``` r
add_fanchart(
  df,
  aes_x = "Time",
  aes_ymin = "Lower",
  aes_ymax = "Upper",
  aes_fill = "Level",
  legend_fill = c("continuous", "discrete"),
  labs_fill = ifelse(legend_fill == "continuous", "Confidence level", "Probability"),
  palette = c("#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD", "#08519C")
)
```

## Arguments

- df:

  Data with columns `aes_x`, `aes_ymin`, `aes_ymax` and `aes_fill`

- aes_x:

  Name of the `x` aesthetic

- aes_ymin:

  Name of the `ymin` aesthetic

- aes_ymax:

  Name of the `ymax` aesthetic

- aes_fill:

  Name of the `fill` aesthetic

- legend_fill:

  Whether the legend should be displayed as `continuous` or as
  `discrete` categories

- labs_fill:

  Name to give to the legend

- palette:

  Colour palette to use. The default is the single-hue blue palette from
  `RColorBrewer::brewer.pal(n = 6, "Blues")`.

## Value

List to be added to a ggplot

## Alternative

A similar result can be obtained using
[`ggdist::geom_lineribbon()`](https://mjskay.github.io/ggdist/), with
the difference that the `ggdist` function also plots a point estimate
(and is a proper geom). To avoid plotting the point estimate, `size` can
be set to 0 and `y = .lower` for example.

## Examples

``` r
library(dplyr)
library(tidyr)
library(ggplot2)

tmp <- tibble(Time = 0:10,
              y = Time^1.5) %>%
  expand_grid(Level = seq(0.1, 0.9, 0.2)) %>%
  mutate(Width = qnorm(0.5 + Level / 2, sd = 2),
         Lower = y - Width,
         Upper = y + Width)
ggplot() + add_fanchart(tmp)

```
