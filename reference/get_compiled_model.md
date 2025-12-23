# Get compiled model

Get compiled model

## Usage

``` r
get_compiled_model(stanmodel)
```

## Arguments

- stanmodel:

  Stan model name. NB: this may differ from the name of the model

## Value

Compiled model (object to pass to
[`rstan::sampling`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html))
