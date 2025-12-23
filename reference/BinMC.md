# Binomial Markov Chain model

This is a state-space model defined by a Binomial measurement error and
a latent Markov Chain. For more details see the BinRW
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

- `sigma`: Standard deviation of the evolution of `ss1`

- `mu_logit_p10`: Population logit mean of `p10`

- `sigma_logit_p10`: Population logit standard deviation of `p10`

### Patient-dependent parameters:

- `p10`: Probability of transitioning from state 1 to state 0

- `logit_p10`: logit of `p10`

- `logit_tss1_0`: Initial condition of the `logit(ss1 * (1 + p10))`

### Observation-dependent (patient- and time-dependent) parameters:

- `p01`: Probability of transitioning from state 0 to state 1

- `lambda`: Mobility of the Markov Chain (eigenvalue of the transition
  matrix)

- `ss1`: Steady state probability of state 1

- `y_lat`: Latent score (probability)

See `list_parameters(model = "BinMC")` for more details.

## Priors

The priors are passed as a named list with elements `sigma`,
`mu_logit_p10` and `sigma_logit_p10` specifying priors for the
corresponding parameters. Each element of the list should be a vector of
length 2, containing values for x1 and x2, x2 \> 0, such as:

- `sigma ~ normal+(x1, x2)`

- `mu_logit_p10 ~ normal(x1, x2)`

- `sigma_logit_p10 ~ normal+(x1, x2)`

- `logit_tss1_0 ~ normal(x1, x2)`

NB: For `sigma` and `sigma_logit_p10`, usually x1=0 to define a
half-normal distribution since the parameter is constrained to be
positive.

## Default priors

- The default prior for `sigma` translates to an odd ratio increment of
  at most 5 (~ 2 \* upper bound of prior).

- The default priors for `mu_logit_p10` and `sigma_logit_p10` translate
  to an approximately uniform prior on `p10`.

- The prior for the initial condition of `ss1` is hard coded and a
  function of `p10`.

## Examples

``` r
EczemaModel("BinMC", max_score = 100)
#> BinMC model (discrete)
#> max_score = 100 
#> Prior: 
#> - sigma ~ normal+(0,0.4)
#> - mu_logit_p10 ~ normal(0,1)
#> - sigma_logit_p10 ~ normal+(0,1.5)
#> - logit_tss1_0 ~ normal(-1,1)
```
