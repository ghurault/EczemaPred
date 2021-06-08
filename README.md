# EczemaPred

<!-- badges: start -->
[![R-CMD-check](https://github.com/ghurault/EczemaPred/workflows/R-CMD-check/badge.svg)](https://github.com/ghurault/EczemaPred/actions)
[![Codecov test coverage](https://codecov.io/gh/ghurault/EczemaPred/branch/main/graph/badge.svg)](https://codecov.io/gh/ghurault/EczemaPred?branch=main)
[![pkgdown](https://github.com/ghurault/EczemaPred/workflows/pkgdown/badge.svg)](https://github.com/ghurault/EczemaPred/actions)
<!-- badges: end -->

EczemaPred is a R package implementing models to serve as building blocks for predicting the evolution of eczema severity, and provides a set of generic functions to manipulate these models.
The models are implemented in the probabilistic programming language [Stan](https://mc-stan.org/).

## Installation

The package requires RStan and C++ toolchain, which can be installed by following these [instructions](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).

Then, the package can be installed by typing the following commands in R:
```
devtools::install_github("ghurault/EczemaPred")
```

The package can take a few minutes to install as the models needs to be compiled (but no compilation will be required when using the package).
Many warnings may be displayed during compilation but they can be safely ignored.

NB: EczemaPred requires [HuraultMisc](https://github.com/ghurault/HuraultMisc), my personal function library, to work.

## Usage

The package is loaded with:
```
library(EczemaPred)
```

The list of functions and datasets is available on the [package website](https://ghurault.github.io/EczemaPred/) or by typing `help(package = "EczemaPred")`.
Examples on how the package can be used are provided in [vignettes](https://ghurault.github.io/EczemaPred/articles/) (long form documentation).

Basic knowledge of Bayesian modelling with Stan and the package [rstan](https://mc-stan.org/users/interfaces/rstan) is required to analyse models' outputs.
The Stan documentation is available [here](https://mc-stan.org/users/documentation/).

NB: While the purpose of the package is to abstract the implementation to the user, the R code and Stan code can be accessed in the [R/](R/) and [inst/stan](inst/stan) directories, respectively.
