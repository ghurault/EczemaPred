# Prior predictive distribution

Prior predictive distribution

## Usage

``` r
sample_prior(model, ...)

# S3 method for class 'EczemaModel'
sample_prior(model, N_patient = 1, t_max = c(2), ...)

# S3 method for class 'MC'
sample_prior(
  model,
  data = data.frame(y0 = integer(), y1 = integer(), dt = integer()),
  ...
)
```

## Arguments

- model:

  Object

- ...:

  Arguments to pass to
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)

- N_patient:

  Number of patients to simulate

- t_max:

  Vector of size `N_patient` indicating the time-series length of each
  patient

- data:

  Dataframe (see details below). Only the columns `y0` and `dt` are
  relevant to simulate data from.

## Value

Object of class stanfit

## Examples

``` r
if (FALSE) { # \dontrun{
model <- EczemaModel("BinRW", max_score = 100)
sample_prior(model)
} # }
if (FALSE) { # \dontrun{
model <- EczemaModel("MC", K = 5)
sample_prior(model)
} # }
```
