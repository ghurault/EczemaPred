# Generate missing values in a times-series

First and last values are nor missing. Missing indices can be generated
at random (Binomial distribution) or using a Markov Chain (if
consecutive missing values are deemed more likely). The markov chain is
parametrised in terms of the steady state probability of a value being
missing and the probability that the next value is observed when the
current value is also observed.

## Usage

``` r
generate_missing(
  N,
  type = c("random", "markovchain"),
  p_mis = 0.25,
  p_obs_obs = 0.75
)
```

## Arguments

- N:

  Length of the time-series

- type:

  Method to generate the missing values. One of "random" or
  "markovchain"

- p_mis:

  Probability of a given value to be missing (steady state probability
  for `type = "markovchain"`)

- p_obs_obs:

  Probability of the next value being observed when the current is
  observed (for `type = "markovchain"`)

## Value

Logical vector of length N

## Examples

``` r
generate_missing(10)
#>  [1] FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE
generate_missing(10, type = "markovchain")
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
```
