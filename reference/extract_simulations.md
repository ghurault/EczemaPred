# Extract simulations

Extract simulations

## Usage

``` r
extract_simulations(fit, id, draw, pars = NULL)
```

## Arguments

- fit:

  Stanfit object

- id:

  Dataframe linking index in Stan model to (Patient, Time) pairs, cf.
  output from
  [`get_index()`](https://ghurault.github.io/EczemaPred/reference/get_index.md)

- draw:

  Draw ID

- pars:

  Vector of parameters to extract. Default to all parameters except
  `y_rep`.

## Value

Named list:

- Data: dataframe with columns Patient, Time, Index, and Score
  corresponding to simulations

- Parameters: dataframe containing parameters used to generate the data
  (cf.
  [`HuraultMisc::extract_draws()`](https://ghurault.github.io/HuraultMisc/reference/extract_draws.html))

## Details

This function is not designed to use with the Markov Chain model (MC).
