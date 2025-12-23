# Print prior distribution

Used internally in the `print.EczemaModel` method.

## Usage

``` r
print_prior(model, ...)

print_distribution(parameter_name, distribution_name, arguments, digits = 2)
```

## Arguments

- model:

  Object

- ...:

  Arguments to pass to other methods

- parameter_name:

  Name of the parameter

- distribution_name:

  Name of the distribution

- arguments:

  Arguments of the distribution (numeric vector)

- digits:

  Number of significant digits to print (cf.
  [`base::signif()`](https://rdrr.io/r/base/Round.html))

## Value

None

## Details

`print_prior` usually calls `print_distribution` with the additional
argument `digits` (except for the base method for `EczemaModel` object).

## Examples

``` r
model <- EczemaModel("BinRW", max_score = 10)
print_prior(model)
#> - sigma ~ normal+(0,0.4)
#> - mu_logit_y0 ~ normal(0,1)
#> - sigma_logit_y0 ~ normal(0,1.5)
print_prior(model, digits = 5)
#> - sigma ~ normal+(0,0.40236)
#> - mu_logit_y0 ~ normal(0,1)
#> - sigma_logit_y0 ~ normal(0,1.5)
print_distribution("x", "normal", c(0, 1))
#> - x ~ normal(0,1)
```
