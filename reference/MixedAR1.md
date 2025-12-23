# Mixed effect autoregressive model (order 1)

Mixed effect autoregressive model (order 1)

## Arguments

- max_score:

  Maximum value that the score can take. Note that even if
  `discrete=FALSE`, `max_score` must be an integer.

- discrete:

  Whether to use a discrete normal distribution. This will be used to
  check whether the data is discrete or not, and for rounding
  predictions (cf. testing).

- prior:

  Named list of the model's priors. If `NULL`, uses the default prior
  for the model (see
  [`default_prior()`](https://ghurault.github.io/EczemaPred/reference/default_prior.md)).

## Details

- Details of the model are available in the [paper](#). The model takes
  as input a continuous score defined between 0 and `max_score`.

- The model is naive as the likelihood is non-truncated and not
  discretised (when `discrete = TRUE`).

- Unlike the `AR1` model, the discretisation of predictions is not
  implemented

- For more details see the
  [vignette](https://ghurault.github.io/EczemaPred/articles/ContinuousModels.html).

## Parameters

### Population parameters:

- `sigma`: Standard deviation of the autoregression

- `mu_logit_slope`: Population mean of the logit of `slope`

- `sigma_logit_slope`: Population standard deviation of the logit of
  `slope`

- `mu_inf`: Population mean of `y_inf`

- `sigma_inf`: Population standard deviation of `y_inf`

### Patient-dependent parameters:

- `slope`: Autocorrelation parameter

- `y_inf`: Autoregression mean

- `intercept`: Intercept

### Other parameters:

- `y_mis`: Missing values

See `list_parameters(model = "MixedAR1")` for more details.

## Priors

The priors are passed as a named list with elements `sigma`,
`mu_logit_slope`, `sigma_logit_slope`, `mu_inf`, `sigma_inf` specifying
priors for the corresponding parameters. Each element of the list should
be a vector of length 2, containing values for x1 and x2, x2 \> 0, such
as:

- `sigma / max_score ~ normal+(x1, x2)`.

- `mu_logit_slope ~ normal(x1, x2)`.

- `sigma_logit_slope ~ normal+(x1, x2)`.

- `mu_inf / max_score ~ normal(x1, x2)`.

- `sigma_inf / max_score ~ normal+(x1, x2)`.

NB: For `sigma`, `sigma_logit_slope` and `sigma_inf`, usually x1=0 to
define a half-normal distribution since the parameter is constrained to
be positive.

## Default priors

- The default prior for `sigma` translates to a width of the predictive
  distribution to be at most `max_score`.

- The default priors for `mu_logit_slope` and `sigma_logit_slope` have
  "reasonable" ranges and translate to a prior on `slope` that is
  approximately uniform.

- The default prior for `mu_inf` spans the entire range of the score.

- The default prior for `sigma_inf` translates to a range in the
  distribution of `y_inf` to be at most `max_score`.

## Examples

``` r
EczemaModel("MixedAR1", max_score = 100)
#> MixedAR1 model (continuous)
#> max_score = 100 
#> Prior: 
#> - sigma / max_score ~ normal+(0,0.1)
#> - mu_logit_slope ~ normal(0,1)
#> - sigma_logit_slope ~ normal+(0,1.5)
#> - mu_inf / max_score ~ normal(0.5,0.25)
#> - sigma_inf / max_score ~ normal+(0.5,0.25)
```
