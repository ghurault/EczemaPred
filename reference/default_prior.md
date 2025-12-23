# Default prior

Default prior

## Usage

``` r
default_prior(model, ...)

# S3 method for class 'character'
default_prior(model, max_score = 2, K = 2, ...)
```

## Arguments

- model:

  Object

- ...:

  Arguments to pass to other methods

- max_score:

  Maximum value that the score can take

- K:

  Number of categories

## Value

Named list of parameters' priors. For more details, see the generic of
the model class.

## Methods (by class)

- `default_prior(character)`: The function creates an EczemaModel object
  and call the corresponding method.

## Examples

``` r
default_prior(EczemaModel("BinRW", max_score = 10))
#> $sigma
#> [1] 0.0000000 0.4023595
#> 
#> $mu_logit_y0
#> [1] 0 1
#> 
#> $sigma_logit_y0
#> [1] 0.0 1.5
#> 
default_prior(EczemaModel("MC", K = 10))
#> $p
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#>  [1,]    1    1    1    1    1    1    1    1    1     1
#>  [2,]    1    1    1    1    1    1    1    1    1     1
#>  [3,]    1    1    1    1    1    1    1    1    1     1
#>  [4,]    1    1    1    1    1    1    1    1    1     1
#>  [5,]    1    1    1    1    1    1    1    1    1     1
#>  [6,]    1    1    1    1    1    1    1    1    1     1
#>  [7,]    1    1    1    1    1    1    1    1    1     1
#>  [8,]    1    1    1    1    1    1    1    1    1     1
#>  [9,]    1    1    1    1    1    1    1    1    1     1
#> [10,]    1    1    1    1    1    1    1    1    1     1
#> 
default_prior("BinRW")
#> default prior for max_score=2 (or, when applicable K=2)
#> $sigma
#> [1] 0.0000000 0.4023595
#> 
#> $mu_logit_y0
#> [1] 0 1
#> 
#> $sigma_logit_y0
#> [1] 0.0 1.5
#> 
```
