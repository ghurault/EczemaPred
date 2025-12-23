# Note to contributors

## Updating Stan files

When updating Stan files, run the following:

    rstantools::rstan_config()
    example(source) # defines the sourceDir() function
    try(roxygen2::roxygenize(load_code = sourceDir), silent = TRUE)
    pkgbuild::compile_dll()
    roxygen2::roxygenize()

## Website

The website is deployed to the “gh-pages” branch of the repository,
using GitHub Actions. To regenerate the website locally, use
[`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html).

## Stan-specific notes

- All `.stan` files containing stanmodel definitions must be placed in
  `inst/stan`.
- Additional files to be included by stanmodel definition files (via
  e.g., `#include "mylib.stan"`) must be placed in any subfolder of
  `inst/stan`.
- Additional C++ files needed by any `.stan` file must be placed in
  `inst/include`, and can only interact with the Stan C++ library via
  `#include` directives placed in the file
  `inst/include/stan_meta_header.hpp`.
- The precompiled stanmodel objects will appear in a named list called
  `stanmodels`, and you can call them with e.g.,
  `rstan::sampling(stanmodels$foo, ...)`

See [Guidelines for developers of R packages interfacing with
Stan](https://mc-stan.org/rstantools/articles/developer-guidelines.html)
for more information.

## Git

I try to follow the [Git
flow](https://www.atlassian.com/git/tutorials/comparing-workflows/gitflow-workflow)
for managing this repository.
