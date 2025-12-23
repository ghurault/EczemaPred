# Add prior to the list serving as input to the Stan sampler

Used internally

## Usage

``` r
add_prior(data_stan, prior, prefix = "prior_")
```

## Arguments

- data_stan:

  List

- prior:

  A named list of corresponding to parameters' prior

- prefix:

  Prefix to add to the names of prior

## Value

data_stan with additional items corresponding to the prior.
