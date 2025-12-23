# Ordered Logistic random walk model

This is a state-space model defined by a Ordered logistic measurement
error distribution and a latent random walk. For more details see the
BinRW
[vignette](https://ghurault.github.io/EczemaPred/articles/BinRW.html).

## Arguments

- max_score:

  Maximum value that the score can take

- prior:

  Named list of the model's priors. If `NULL`, uses the default prior
  for the model (see
  [`default_prior()`](https://ghurault.github.io/EczemaPred/reference/default_prior.md)).

## Details

Details of the model are available in the [paper](#).

## Parameters

### Population parameters:

- `sigma_lat`: Standard deviation of the random walk

- `sigma_meas`: Standard deviation (not scale) of the logistic
  distribution (in `[0, max_score]` space)

- `sigma_tot`: Total standard deviation for prediction one step ahead

- `rho2`: Proportion of measurement variance to the total variance. It
  can be interpreted similarly to an R-squared, the proportion of the
  explained variance (the variance of the measurements) in the total
  variance.

- `mu_y0`: Population mean of `y0` (initial condition).

- `sigma_y0`: Population standard deviation of `y0` (initial condition).

- `delta`: Relative difference between cutpoints (simplex of length
  `max_score - 1`)

- `ct`: Cutpoints (vector of length `max_score`, in `[0, max_score]`
  space)

### Patient-dependent parameters:

- `y0`: initial latent score (`y_lat` at t0).

### Observation-dependent (patient- and time-dependent) parameters:

- `y_lat`: Latent score (in `[0, max_score]` space)

See `list_parameters(model = "OrderedRW")` for more details.

## Priors

The priors are passed as a named list with elements `delta`,
`sigma_lat`, `sigma_meas`, `mu_y0` and `sigma_y0` specifying priors for
the corresponding parameters.

The element `delta` should be a vector X1 of length `max_score - 1`,
such as all all elements of X1 are positive and `delta ~ dirichlet(X1)`.

The latent score can be interpreted in the original `[0, max_score]`
space, the priors for the other parameters are specified normalised
`max_score`. Their priors are defined by a vector of length 2,
containing values for x1 and x2, x2 \> 0, such as:

- `sigma_meas / max_score ~ lognormal(x1, x2)`

- `sigma_lat / max_score ~ lognormal(x1, x2)`

- `mu_y0 ~ normal(x1, x2)`

- `sigma_y0 ~ normal+(x1, x2)`

NB: for the lognormal distribution, x1 corresponds to the mean of the
log and x2 to the sd of the log. NB: `sigma_y0` is constrained to be
positive so x1 are usually set to 0 to define a half-normal
distribution.

## Default priors

- The default prior for `delta` is a uniform symmetric Dirichlet
  distribution with concentration 2.

- The default priors for `sigma_meas` and `sigma_lat` are lognormal
  distribution which translate to a 95% CI that is approximately
  `[.02, 0.40] * M`. The prior for `sigma_lat` thus allows fast or slow
  transitions from a state where `y = 0` is the most likely outcome to a
  state where `y = M` is the most likely outcome. The prior for
  `sigma_meas` allows very precise or imprecise measurements.

- The default priors for `mu_y0` and `sigma_y0` have reasonable ranges
  and translate to an approximately uniform prior over the range of the
  score for `y0`.

## Examples

``` r
EczemaModel("OrderedRW", max_score = 10)
#> OrderedRW model (discrete)
#> max_score = 10 
#> Prior: 
#> - delta ~ dirichlet(2,2,2,2,2,2,2,2,2)
#> - sigma_meas / max_score ~ lognormal(-2.3,0.69)
#> - sigma_lat / max_score ~ lognormal(-2.3,0.69)
#> - mu_y0 / max_score ~ normal(0.5,0.25)
#> - sigma_y0 / max_score ~ normal+(0,0.12)
```
