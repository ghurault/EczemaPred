# List available parameters

List available parameters

## Usage

``` r
list_parameters(model, ...)

# S3 method for class 'BinRW'
list_parameters(model, main = TRUE, ...)

# S3 method for class 'BinMC'
list_parameters(model, main = TRUE, ...)
```

## Arguments

- model:

  Object, usually the model name or an EczemaModel object

- ...:

  Arguments to pass to other methods

- main:

  Whether to output the main parameters only (when applicable).

## Value

Named list of parameters names, grouped into broad categories:

- `Population`: population parameters (i.e. patient- and
  time-independent)

- `Patient`: patient-dependent parameters

- `PatientTime`: patient- and time-dependent parameters (e.g. latent
  scores)

- `Test`: parameters related to the test set

- `Misc`: other parameters

## Details

See [MC](https://ghurault.github.io/EczemaPred/reference/MC.md),
[BinRW](https://ghurault.github.io/EczemaPred/reference/BinRW.md),
[BinMC](https://ghurault.github.io/EczemaPred/reference/BinMC.md),
[OrderedRW](https://ghurault.github.io/EczemaPred/reference/OrderedRW.md),
[RW](https://ghurault.github.io/EczemaPred/reference/RW.md),
[Smoothing](https://ghurault.github.io/EczemaPred/reference/Smoothing.md),
[AR1](https://ghurault.github.io/EczemaPred/reference/AR1.md) and
[MixedAR1](https://ghurault.github.io/EczemaPred/reference/MixedAR1.md)
for details about the model-specific parameters. Other parameters are
available across models:

- `y_rep` correspond to posterior replications. To get the corresponding
  index, use
  [`get_index()`](https://ghurault.github.io/EczemaPred/reference/get_index.md).

- `y_pred` is a subset of y_rep corresponding to test samples (size
  `N_test` equal to the number of observations in the test set).

- `lpd` is the log predictive density of test samples (of size
  `N_test`).

- `cum_err` is the cumulative error distribution, only available for
  discrete outcomes (matrix with dimensions `N_test * (max_score + 1)`).

## Examples

``` r
list_parameters("RW")
#> $Population
#> [1] "sigma"
#> 
#> $PatientTime
#> [1] "y_rep"
#> 
#> $Test
#> [1] "y_pred" "lpd"   
#> 
#> $Misc
#> [1] "y_mis"
#> 
list_parameters(EczemaModel("RW", max_score = 100))
#> $Population
#> [1] "sigma"
#> 
#> $PatientTime
#> [1] "y_rep"
#> 
#> $Test
#> [1] "y_pred" "lpd"   
#> 
#> $Misc
#> [1] "y_mis"
#> 
list_parameters(EczemaModel("BinRW", max_score = 100))
#> $Population
#> [1] "sigma"          "mu_logit_y0"    "sigma_logit_y0"
#> 
#> $Patient
#> [1] "logit_y0"
#> 
#> $PatientTime
#> [1] "y_lat" "y_rep"
#> 
#> $Observation
#> [1] "log_lik"
#> 
#> $Test
#> [1] "y_pred"  "lpd"     "cum_err"
#> 
list_parameters(EczemaModel("BinMC", max_score = 100))
#> $Population
#> [1] "mu_logit_p10"    "sigma_logit_p10" "sigma"          
#> 
#> $Patient
#> [1] "p10"
#> 
#> $PatientTime
#> [1] "p01"    "lambda" "ss1"    "y_lat"  "y_rep" 
#> 
#> $Test
#> [1] "y_pred"  "lpd"     "cum_err"
#> 
```
