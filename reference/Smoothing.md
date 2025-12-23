# Exponential smoothing model

Exponential smoothing model

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

- `alpha`: Smoothing factor

- `tau`: Time constant associated with the smoothing factor

- `y_mis`: Missing values

See `list_parameters(model = "Smoothing")` for more details.

## Priors

The priors are passed as a named list with elements `sigma` and `tau`
specifying priors for the corresponding parameters. Each element of the
list should be a vector of length 2, containing values for x1 and x2, x2
\> 0, such as:

- `sigma / max_score ~ normal+(x1, x2)`.

- `tau ~ lognormal(x1, x2)`.

NB: For `sigma`, usually x1=0 to define a half-normal distribution since
the parameter is constrained to be positive.

## Default priors

- The default prior for `sigma` translates to a width of the predictive
  distribution to be at most `max_score`.

- The default prior for `tau` assumes it could range from less a 1 to
  100 (time units).

## Examples

``` r
EczemaModel("Smoothing", max_score = 100)
#> Smoothing model (continuous)
#> max_score = 100 
#> Prior: 
#> - sigma / max_score ~ normal+(0,0.1)
#> - tau ~ lognormal(1.2,1.7)
```
