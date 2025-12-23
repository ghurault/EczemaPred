# Create "empty" train and test set to pass to `prepare_standata` for `sample_prior`

Used internally.

## Usage

``` r
make_empty_data(N_patient = 1, t_max = c(2), max_score, discrete)
```

## Arguments

- N_patient:

  Number of patients in the datasets

- t_max:

  Vector of size `N_patient` indicating the time-series length of each
  patient

- max_score:

  Maximum value that the score can take

- discrete:

  Whether to use a discrete normal distribution (only relevant for
  testing)

## Value

List containing "Training" and "Testing" dataframes

## Examples

``` r
make_empty_data(max_score = 10, discrete = TRUE)
#> $Training
#> # A tibble: 1 × 3
#>   Patient  Time Score
#>     <int> <dbl> <dbl>
#> 1       1     1     8
#> 
#> $Testing
#> # A tibble: 1 × 3
#>   Patient  Time Score
#>     <int> <dbl> <dbl>
#> 1       1     2     0
#> 
```
