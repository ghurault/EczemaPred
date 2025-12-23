# Replace prior

Used internally to overwrite default prior in the constructor. Beware
that the validity of the new prior is not tested in this function, you
may want to call
[`validate_prior()`](https://ghurault.github.io/EczemaPred/reference/validate_prior.md)
after using this function.

## Usage

``` r
replace_prior(x, prior = NULL)
```

## Arguments

- x:

  EczemaModel object

- prior:

  Named list of the model's prior to replace. If NULL, the prior stays
  the same

## Value

Object of the same class as `x`

## Examples

``` r
model <- EczemaModel("OrderedRW", max_score = 5)
print(model)
#> OrderedRW model (discrete)
#> max_score = 5 
#> Prior: 
#> - delta ~ dirichlet(2,2,2,2)
#> - sigma_meas / max_score ~ lognormal(-2.3,0.69)
#> - sigma_lat / max_score ~ lognormal(-2.3,0.69)
#> - mu_y0 / max_score ~ normal(0.5,0.25)
#> - sigma_y0 / max_score ~ normal+(0,0.12)
replace_prior(model, prior = list(sigma = c(0, 1)))
#> Warning: The following parameters do not exist or their priors do not need to be specified: sigma
#> OrderedRW model (discrete)
#> max_score = 5 
#> Prior: 
#> - delta ~ dirichlet(2,2,2,2)
#> - sigma_meas / max_score ~ lognormal(-2.3,0.69)
#> - sigma_lat / max_score ~ lognormal(-2.3,0.69)
#> - mu_y0 / max_score ~ normal(0.5,0.25)
#> - sigma_y0 / max_score ~ normal+(0,0.12)
```
