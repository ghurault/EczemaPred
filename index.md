# EczemaPred

EczemaPred is a R package implementing models to serve as building
blocks for predicting the evolution of eczema severity, and provides a
set of generic functions to manipulate these models. The models are
implemented in the probabilistic programming language
[Stan](https://mc-stan.org/).

EczemaPred was first introduced in [**Hurault et al. (2022),
“EczemaPred: A computational framework for personalised prediction of
eczema severity dynamics”**](https://doi.org/10.1002/clt2.12140),
published in Clinical and Translational Allergy. The analysis code of
this research article is available
[here](https://github.com/ghurault/EczemaPredPOSCORAD).

## Installation

The package requires RStan and C++ toolchain, which can be installed by
following these
[instructions](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).

Then, the package can be installed by typing the following commands in
R:

    devtools::install_github("ghurault/EczemaPred")

Or to install a specific version, for example the initial release
(v0.1.0):

    devtools::install_github("ghurault/EczemaPred@v0.1.0")

The package can take a few minutes to install as the models needs to be
compiled (but no compilation will be required when using the package).
Many warnings may be displayed during compilation but they can be safely
ignored.

NB: EczemaPred requires
[HuraultMisc](https://github.com/ghurault/HuraultMisc), my personal
function library, to work.

## Usage

The package is loaded with:

    library(EczemaPred)

If you are working on a local, multicore CPU with excess RAM, you may
want to call `options(mc.cores = parallel::detectCores())` to run Stan
on multiple cores on parallel.

The list of functions and datasets is available on the [package
website](https://ghurault.github.io/EczemaPred/) or by typing
[`help(package = "EczemaPred")`](https://ghurault.github.io/EczemaPred/reference).
Examples on how the package can be used are provided in
[vignettes](https://ghurault.github.io/EczemaPred/articles/) (long form
documentation).

Basic knowledge of Bayesian modelling with Stan and the package
[rstan](https://mc-stan.org/users/interfaces/rstan) is required to
analyse models’ outputs. The Stan documentation is available
[here](https://mc-stan.org/users/documentation/).

NB: While the purpose of the package is to abstract the implementation
to the user, the R code and Stan code can be accessed in the
[R/](https://ghurault.github.io/EczemaPred/R/) and
[inst/stan](https://ghurault.github.io/EczemaPred/inst/stan)
directories, respectively.

## License

The open source version of EczemaPred is licensed under the GPL v3
license, which can be seen in the
[LICENSE](https://ghurault.github.io/EczemaPred/LICENSE.md) file. A
closed source version of EczemaPred is also available without the
restrictions of the GPL v3 license with a software usage agreement from
Imperial College London. For more information, please contact [Diana
Yin](mailto:d.yin@imperial.ac.uk).
