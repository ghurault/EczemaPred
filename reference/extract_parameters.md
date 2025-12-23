# Extract parameters posterior summary statistics

Extract parameters posterior summary statistics

## Usage

``` r
extract_parameters(fit, pars = NULL, id = NULL, ...)
```

## Arguments

- fit:

  Stanfit object

- pars:

  Named list of parameters to extract. See
  [`list_parameters()`](https://ghurault.github.io/EczemaPred/reference/list_parameters.md).
  If `NULL`, it extracts all parameters of the model.

- id:

  Dataframe associating Index to (Patient, Time) pairs. See
  [`get_index()`](https://ghurault.github.io/EczemaPred/reference/get_index.md).
  `id` is not used when `pars = NULL` or `pars` does not contain an
  element `PatientTime`.

- ...:

  Arguments to pass to
  [`HuraultMisc::summary_statistics()`](https://ghurault.github.io/HuraultMisc/reference/summary_statistics.html).

## Value

Tibble dataframe containing posterior summary statistics. See details in
[`HuraultMisc::summary_statistics()`](https://ghurault.github.io/HuraultMisc/reference/summary_statistics.html).
Additional columns Patient and Time if `id` is not NULL.

## Examples

``` r
if (FALSE) { # \dontrun{
model <- EczemaModel("BinRW", max_score = 100)
id <- get_index2(rpois(10, 20))
train <- id %>% mutate(Score = rbinom(nrow(.), 100, .5))
fit <- EczemaFit(model, train)
extract_parameters(fit, pars = list_parameters("BinRW"), id = id)
} # }
```
