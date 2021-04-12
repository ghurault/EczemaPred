# EczemaPred

<!-- badges: start -->
[![R-CMD-check](https://github.com/ghurault/EczemaPred/workflows/R-CMD-check/badge.svg)](https://github.com/ghurault/EczemaPred/actions)
[![Codecov test coverage](https://codecov.io/gh/ghurault/EczemaPred/branch/main/graph/badge.svg)](https://codecov.io/gh/ghurault/EczemaPred?branch=main)
<!-- badges: end -->

EczemaPred is a R package implementing models to serve as building blocks for predicting the evolution severity, and provides a set of generic functions to manipulate these models.
The models are implemented in the probabilistic programming language [Stan](https://mc-stan.org/).

## Installation

The package requires RStan and C++ toolchain, which can be installed by following these [instructions](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).

Then, the package can be installed by typing the following commands in R:
```
devtools::install_github("ghurault/EczemaPred")
```

NB: EczemaPred requires [HuraultMisc](https://github.com/ghurault/HuraultMisc), my personal function library, to work.

## Usage

The package is loaded with:
```
library(EczemaPred)
```

The list of functions and datasets is available on the [package website]() or by typing `help(package = "EczemaPred")`.
Examples on how the package can be used are provided in [vignettes]() (long form documentation).

NB: While the purpose of the package is to abstract the implementation to the user, the R code and Stan code can be accessed in the [R/](R/) and [inst/stan](inst/stan) directories, respectively.
