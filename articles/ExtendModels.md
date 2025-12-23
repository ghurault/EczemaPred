# Extending EczemaPred models

## Introduction

The EczemaPred package provides a collection of “simple” statistical
models that addresses common challenges when working with observational
eczema severity data:

- multiple time-series (longitudinal data) and potentially
  patient-dependent parameters
- irregular measurements or missing values
- imprecise measurements (cf. measurement error)
- small data (cf. uncertainty quantification)
- discrete ordinal (notably bounded) outcomes
- severity scores that are aggregates of multiple items

Then, these models can be extended to investigate diverse research
questions.

This vignette describes how we use the package to modify the
pre-processing when a model has been extended.

``` r
library(EczemaPred)
library(dplyr)
```

## Overview

The package uses the S3 object oriented programming system in R to allow
inheritance and polymorphism. For more details of the S3 OOP system, you
can check [Hadley Wickham’s book](https://adv-r.hadley.nz/s3.html).

The package provides a set of generics that can be useful when extending
a models:

- [`default_prior()`](https://ghurault.github.io/EczemaPred/reference/default_prior.md):
  Provide default priors
- [`validate_prior()`](https://ghurault.github.io/EczemaPred/reference/validate_prior.md):
  Check that the prior is correctly specified
- [`prepare_standata()`](https://ghurault.github.io/EczemaPred/reference/prepare_standata.md):
  Prepare data input to the Stan sampler
- [`list_parameters()`](https://ghurault.github.io/EczemaPred/reference/list_parameters.md):
  List parameters of the model
- [`print_prior()`](https://ghurault.github.io/EczemaPred/reference/print_prior.md):
  How to display priors in the `print` method (optional)
- [`EczemaFit()`](https://ghurault.github.io/EczemaPred/reference/EczemaFit.md):
  Fit model (default generic is only valid with models provided in the
  package)
- [`sample_prior()`](https://ghurault.github.io/EczemaPred/reference/sample_prior.md):
  Sample prior predictive distribution of the model (default generic is
  only valid with models provided in the package)

## Example

For example, let’s assume we would like to include additional covariates
in the `BinRW` model, such as the sex of the patient (variable `sex`,
coded as binary, with coefficient `beta_sex`).

### Modifying the Stan code

In the original Stan model, available
[here](https://github.com/ghurault/EczemaPred/blob/main/inst/stan/BinRW.stan),
this would mean:

- Including `sex` in the data block:
  `int<lower = 0, upper = 1> sex[N_pt];`
- Including `beta_sex` in the parameters block: `real beta_sex`;
- Changing the latent dynamic in the transformed parameters block,
  i.e. changing `logit_lat[t] = logit_lat[t - 1] + sigma * eta[t];` to
  `logit_lat[t] = logit_lat[t - 1] + beta_sex * sex[k] sigma * eta[t];`
- Include the prior for `beta_sex` in the model block, for example:
  `sex ~ normal(0, 1)`. The prior can also be specified as data in the
  data block with `real prior_beta_sex[2];` and in the model block
  `sex ~ normal(prior_beta_sex[1], prior_beta_sex[2]);`

Let’s call this new model `BinRW2`.

### Updating the pre-processing

One way to use the package infrastructure to minimise the amount of new
code to write would be to create a new model class `BinRW2` that
inherits from the class `BinRW`. We also write `BinRW2` methods for
`default_prior`, `validate_prior` and `print_prior` which makes use of
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) to call the
methods for the parent class `BinRW`.

``` r
BinRW2 <- function(max_score, prior = NULL) {
  
  model <- EczemaModel("BinRW", max_score = max_score)
  
  model$name <- "BinRW2"
  model$stanmodel <- "path_to_stancode"
  class(model) <- c("BinRW2", class(model))
  
  model$prior <- default_prior(model) # Overwrite BinRW prior
  model <- replace_prior(model, prior = prior) # Overwrite BinRW2 default prior
  validate_prior(model) # Check BinRW2 prior
  
  return(model)

}

default_prior.BinRW2 <- function(model) {
  c(list(beta_sex = c(0, 1)),
    NextMethod())
}

validate_prior.BinRW2 <- function(model) {
  prior <- model$prior
  stopifnot(is.list(prior),
            "beta_sex" %in% names(prior),
            is.numeric(prior$beta_sex),
            length(prior$beta_sex) == 2,
            prior$beta_sex[2] > 0)
  NextMethod()
}

print_prior.BinRW2 <- function(model, digits = 2) {
  print_distribution("beta_sex", "normal", model$prior$beta_sex, digits = digits)
  NextMethod()
}
```

Similarly we can write the method for `list_parameters`:

``` r
list_parameters.BinRW2 <- function(model) {
  pars <- NextMethod()
  pars$Population <- c(pars$Population, "beta_sex")
  return(pars)
}
```

And finally, we need to write a method for `prepare_standata`, where we
only need to provide the `sex` data in addition to the data required for
“BinRW”.

``` r
prepare_standata.BinRW2 <- function(model, train, test = NULL, sex) {
  out <- NextMethod()
  stopifnot(out$N_pt == length(sex),
            all(sex %in% c(0, 1)))
  out <- c(out, list(sex = sex))
  return(out)
}
```

### Fitting the model

Now that everything is in place, we can use the above generics to sample
the prior predictive distribution of the model or fit to data like the
EczemaPred models.

``` r
max_score <- 100

(model <- BinRW2(max_score = max_score))
#> BinRW2 model (discrete)
#> max_score = 100 
#> Prior: 
#> - beta_sex ~ normal(0,1)
#> - sigma ~ normal+(0,0.4)
#> - mu_logit_y0 ~ normal(0,1)
#> - sigma_logit_y0 ~ normal(0,1.5)

list_parameters(model)
#> $Population
#> [1] "sigma"          "mu_logit_y0"    "sigma_logit_y0" "beta_sex"      
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
```

To sample the prior predictive distribution, we need to generate an
empty dataset to pass to `prepare_standata` and then call
[`rstan::stan`](https://mc-stan.org/rstan/reference/stan.html) (the code
could be put into a function `sample_prior.BinRW2`).

``` r
N_patient <- 10
t_max <- rpois(N_patient, 20)
sex <- rbinom(N_patient, 1, 0.5)

tmp <- make_empty_data(N_patient = N_patient,
                       t_max = t_max,
                       max_score = model$max_score,
                       discrete = model$discrete)

data_stan <- prepare_standata(model, train = tmp[["Training"]], test = tmp[["Testing"]], sex = sex) %>%
  c(list(run = 0))

if (FALSE) {
  fit_prior <- rstan::stan(model$stanmodel, data = data_stan, ...)
}
```

To fit the model to real data, assuming we have a training set and
optionally a testing set, we would just need to call `prepare_standata`
and then [`rstan::stan`](https://mc-stan.org/rstan/reference/stan.html)
(this equivalent to the `EczemaFit` function).

``` r
if (FALSE) {
  train <- get_index2(t_max) %>%
    mutate(Score = rstan::extract(fit_prior, pars = "y_rep")[[1]][10, ]) # fake data for the example
  
  data_stan <- prepare_standata(model, train = train, sex = sex) %>%
    c(list(run = 1))
  
  fit <- rstan::stan(model$stanmodel, data = data_stan, ...)
}
```
