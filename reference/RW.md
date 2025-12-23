# Random walk model

Random walk model

## Arguments

- max_score:

  Maximum value that the score can take

- discrete:

  Whether to use a discrete normal distribution. This will be used to
  check whether the data is discrete or not, and for rounding
  predictions (cf. testing).

- prior:

  Named list of the model's priors. If `NULL`, uses the default prior
  for the model (see
  [`default_prior()`](https://ghurault.github.io/EczemaPred/reference/default_prior.md)).

## Details

- Details of the model are available in the [paper](#).

- The model takes as input a continuous score defined between 0 and
  `max_score`.

- The model is naive as the likelihood is non-truncated and not
  discretised (when `discrete = TRUE`). As a result, sampling from the
  prior predictive distribution can be challenging if the score is near
  the bounds and the variance is sufficiently large.

- For more details see the
  [vignette](https://ghurault.github.io/EczemaPred/articles/ContinuousModels.html).

## Parameters

- `sigma`: Standard deviation of the random walk

- `y_mis`: Missing values

See `list_parameters(model = "RW")` for more details.

## Priors

The priors are passed as a named list with element `sigma` specifying
priors for the corresponding parameter, where
`sigma / max_score ~ normal+(x1, x2)` and the element `sigma` of the
list is a vector of length two containing x1 and x2. NB: usually x1=0 to
define a half-normal distribution (sigma is constraint to be positive)
and x2 should be positive.

## Default priors

The default prior for `sigma` translates to a width of the predictive
distribution to be at most `max_score`.

## Examples

``` r
EczemaModel("RW", max_score = 100, discrete = FALSE)
#> RW model (continuous)
#> max_score = 100 
#> Prior: 
#> - sigma / max_score ~ normal+(0,0.1)
```
