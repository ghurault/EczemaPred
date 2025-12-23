# Extract lpd (predictive log-likelihood) and RPS from stanfit object

The metrics are computed for the expected forecast distribution. The lpd
is defined for continuous and discrete outcomes. The RPS is defined for
discrete outcomes only and is computed by extracting the cumulative
error distribution (`cum_err`: cumulative forecast - cumulative
distribution), taking its expected value (cf. expected forecast),
squaring it and apply, summing over possible outcomes and normalising by
the number of outcomes `- 1`.

## Usage

``` r
extract_loglikelihood(fit, par_name = "log_lik")

extract_lpd(fit)

extract_RPS(fit, par_name = "cum_err")
```

## Arguments

- fit:

  Stanfit object

- par_name:

  Name of the parameter to parameter in the Stan model. Usually `lpd`,
  `log_lik` (for the log likelihood of the data) or `cum_err`.

## Value

Vector of lpd/RPS for each prediction
