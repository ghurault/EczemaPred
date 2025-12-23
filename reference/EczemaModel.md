# EczemaModel constructor

EczemaModel constructor

## Usage

``` r
EczemaModel(
  model_name = c("BinRW", "OrderedRW", "BinMC", "RW", "Smoothing", "AR1", "MixedAR1",
    "MC"),
  max_score = NULL,
  K = NULL,
  discrete = FALSE,
  prior = NULL
)
```

## Arguments

- model_name:

  Name of the model to create

- max_score:

  Maximum value that the score can take. Required for all models except
  "MC".

- K:

  Number of categories. Only required for "MC" model.

- discrete:

  Whether the model is discrete or not. Only required for "RW".

- prior:

  Named list of the model's priors. It uses the default priors (see
  [`default_prior()`](https://ghurault.github.io/EczemaPred/reference/default_prior.md))
  if `NULL` and for the parameters that are not provided.

## Value

An object (list) of class `model_name` and EczemaModel, with elements:

- `model_name`: Name of the model

- `stanmodel`: Name of the Stan model. Used internally to locate the
  compiled code. It can also be used to store the Stan code filepath.

- `discrete`: Whether the model is discrete or not.

- `max_score`: Maximum value that the score can take (when applicable)

- `K`: Number of categories (when applicable)

- `prior`: List of parameters' priors

## Examples

``` r
EczemaModel("BinRW", max_score = 10)
#> BinRW model (discrete)
#> max_score = 10 
#> Prior: 
#> - sigma ~ normal+(0,0.4)
#> - mu_logit_y0 ~ normal(0,1)
#> - sigma_logit_y0 ~ normal(0,1.5)
```
