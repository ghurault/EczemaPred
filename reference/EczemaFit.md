# Fit an EczemaModel

Fit an EczemaModel

## Usage

``` r
EczemaFit(model, train, test, ...)

# S3 method for class 'EczemaModel'
EczemaFit(model, train, test = NULL, ...)
```

## Arguments

- model:

  Object

- train:

  Training dataframe (see details below)

- test:

  Testing dataframe (see details below)

- ...:

  Arguments to pass to
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)

## Value

Stanfit object

## Data format

### All models except "MC"

- `train` and `test` should have the columns `Patient` (patient ID),
  `Time` (timepoint) and `Score` (score to model).

- `Patient` should take integer values between 1 and the number of
  patients in the training set.

- `Time` should take integer (so discrete) values and starts with one
  for every patient.

- `Score` should take values between 0 and max_score.

- Missing values are not allowed (but Time values are not necessarily
  consecutive, for example if Score at t=5 is missing, but not at t=4
  and t=6, just remove t=5).

### "MC" model

- `train` and `test` should have columns `y0` (for the current state),
  `y1` (for the next state) and `dt` (for the time delay between
  states).

- `y0` and `y1` should take integer values between 1 and K.

- `dt` should take integer values greater than or equal to 1.

- Missing values are not allowed.

## Examples

``` r
if (FALSE) { # \dontrun{
model <- EczemaModel("BinRW", max_score = 100)
train <- data.frame(Patient = 1, Time = 1:10, Score = rbinom(10, 100, .5))
EczemaFit(model, train)
} # }
```
