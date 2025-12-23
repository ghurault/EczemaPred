# Append lpd and (C)RPS to (test) dataframe

- `add_metrics1_d()` and `add_metrics1_c()` extracts the lpd and RPS
  from the Stanfit object

- `add_metrics2_d()` and `add_metrics2_c()` calculates the lpd and
  (C)RPS from the empirical pmf

- The metrics in `add_metrics2_c()` and the CRPS of `add_metrics1_c()`
  are calculated using the `scoringRules` package.

## Usage

``` r
add_metrics1_d(df, fit)

add_metrics1_c(df, fit)

add_metrics2_d(df, support, add_samples = support)

add_metrics2_c(df, add_samples = NULL, bw = NULL)
```

## Arguments

- df:

  Dataframe to add the metrics to

  - For `add_metrics1_c()`, it must contain a column "Score".

  - For `add_metrics2_c()` and `add_metrics2_d()`, it must contain the
    columns "Samples" and "Score".

- fit:

  Stanfit object with parameters "lpd", and for `add_metrics1_d()`
  "cum_err".

- support:

  Support of the distribution

- add_samples:

  Numeric vector used to initialise the distribution when computing the
  lpd and (C)RPS. For example, this can be used to add a uniform
  distribution to the vector of samples, to avoid problems at the tail
  of the distribution. If `NULL`, the empirical pmf is not changed.
  Default to the uniform distribution (i.e. `support`) for
  `add_metrics2_d()` and `NULL` for `add_metrics2_c()`. The column
  "Samples" is not modified when `add_samples` is not NULL.

- bw:

  Bandwidth, for calculating lpd, see
  [`scoringRules::logs_sample()`](https://rdrr.io/pkg/scoringRules/man/scores_sample_univ.html).
  Useful to set the "resolution" of the distribution.

## Value

Dataframe `df` appended by the columns "lpd", "RPS" (or "CRPS" for
`add_metrics1_c()` and `add_metrics2_d()`).

## See also

[`extract_lpd()`](https://ghurault.github.io/EczemaPred/reference/extract_metric.md),
[`extract_RPS()`](https://ghurault.github.io/EczemaPred/reference/extract_metric.md),
[`HuraultMisc::compute_RPS()`](https://ghurault.github.io/HuraultMisc/reference/compute_RPS.html),
[`scoringRules::logs_sample()`](https://rdrr.io/pkg/scoringRules/man/scores_sample_univ.html),
[`scoringRules::crps_sample()`](https://rdrr.io/pkg/scoringRules/man/scores_sample_univ.html).
