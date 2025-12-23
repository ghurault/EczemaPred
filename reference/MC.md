# Markov Chain model

For more details see the [Markov Chain
vignette](https://ghurault.github.io/EczemaPred/articles/MC.html).

## Arguments

- K:

  Number of states of the Markov Chain

- prior:

  Named list of the model's priors. If `NULL`, uses the default prior
  for the model (see
  [`default_prior()`](https://ghurault.github.io/EczemaPred/reference/default_prior.md)).

## Details

Details of the model are available in the [paper](#).

## Parameters

- `p`: matrix of size K \* K where `p[i, j]` represents the transition
  probabilities from state i to state j.

See `list_parameters(model = "MC")` for more details.

## Priors

The priors are passed as a named list with element `p`. The transition
probabilities from state i `p[i, ]` are assumed to follow a Dirichlet
distribution. The prior should be a matrix where each line correspond to
the parameters of the Dirichlet distribution for `p[i, ]`.

## Default priors

The default prior for all `p[i, ]` is a symmetric uniform Dirichlet
distribution (all concentration parameters are equal to 1).

## Examples

``` r
EczemaModel("MC", K = 5)
#> MC model (discrete)
#> 5 categories 
#> Prior: 
#> - p[1, ] ~ dirichlet(1,1,1,1,1)
#> - p[2, ] ~ dirichlet(1,1,1,1,1)
#> - p[3, ] ~ dirichlet(1,1,1,1,1)
#> - p[4, ] ~ dirichlet(1,1,1,1,1)
#> - p[5, ] ~ dirichlet(1,1,1,1,1)
```
