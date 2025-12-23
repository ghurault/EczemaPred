# Prepare the data list to pass to the Stan sampler

Used internally.

## Usage

``` r
prepare_standata(model, train, test, ...)

prepare_data_lgtd(train, test = NULL, max_score, discrete)
```

## Arguments

- model:

  Object

- train:

  Training dataframe (details of the format in
  [`EczemaFit()`](https://ghurault.github.io/EczemaPred/reference/EczemaFit.md))

- test:

  Testing dataframe (details of the format in
  [`EczemaFit()`](https://ghurault.github.io/EczemaPred/reference/EczemaFit.md))

- ...:

  Arguments to pass to other methods

- max_score:

  Maximum value that the score can take

- discrete:

  Whether to use a discrete normal distribution (only relevant for
  testing)

## Value

List to serve as input to the Stan sampler. The list is usually
incomplete needs to be optional parameters, such as:

- `run` (binary, for main and MC models, indicating whether to evaluate
  the likelihood)

## Details

- `prepare_data_lgtd` is helps build `prepare_standata.EczemaModel` and
  is kept for compatibility reasons. The list that it outputs does not
  include the priors.
