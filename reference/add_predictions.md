# Append lpd, (C)RPS and predictive samples to (test) dataframe

Append lpd, (C)RPS and predictive samples to (test) dataframe

## Usage

``` r
add_predictions(
  df,
  fit,
  discrete = TRUE,
  include_samples = FALSE,
  n_samples = NULL
)
```

## Arguments

- df:

  Dataframe. When `discrete = FALSE`, it must contain a column "Score".

- fit:

  Stanfit object

- discrete:

  Whether to estimate a discrete or continuous forecast. For a discrete
  forecast, the RPS will be computed and the CRPS for a continuous
  forecast.

- include_samples:

  Whether to return samples from the historical forecast in the output

- n_samples:

  If `include_samples=TRUE`, how many samples to return. Default (=NULL)
  to all samples.

## Value

Dataframe `df` appended by the columns "lpd", "RPS" (or CRPS if
`discrete=FALSE`) and optionally "Samples"

## See also

[`add_metrics1_d()`](https://ghurault.github.io/EczemaPred/reference/add_metrics.md)
and
[`add_metrics1_c()`](https://ghurault.github.io/EczemaPred/reference/add_metrics.md)
